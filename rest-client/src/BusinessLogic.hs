{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BusinessLogic where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.ByteString (ByteString)
import           Data.Int
import qualified Data.List                       as L
import           Data.Maybe
import qualified Data.Text                        as T
import           Data.Time                        (UTCTime)
import           Data.Typeable
import           Snap.Snaplet
import           Snap.Snaplet.SqliteSimple

import qualified Database.SQLite.Simple           as S
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.Internal
import           Database.SQLite.Simple.Ok

import           Application
import           Messages.Types

data Credential = Credential
                { credentialCode :: Maybe Int64
                , credentialCred :: T.Text
                } deriving (Eq, Show)

data CredentialAuthor = CredentialAuthor
                { credAuthorExp :: UTCTime
                } deriving (Eq, Show)

data User = User
          { userCode        :: Maybe Int64
          , userLogin       :: T.Text
          , userPassword    :: T.Text
          , userCredentials :: [Credential]
          } deriving (Eq, Show)

data Contract = Contract
              { contrCode     :: Maybe Int64 
              , contrUsers    :: [User]
              , contrServices :: [Service]
              } deriving (Eq, Show)

data Service = Service
             { servCode :: Maybe Int64
             , servURL  :: URI
             } deriving (Show)

instance Eq Service where
    s1 == s2 = servURL s1 == servURL s2

findCredentialById :: Int64 -> Handler App Sqlite (Maybe Credential)
findCredentialById credCode = do
    r <- query "SELECT credencial FROM tb_credencial WHERE cod_credencial = ?" (Only credCode)
    case r of
         [Only cred :: Only T.Text] -> return $ Just $ Credential (Just credCode) cred
         _            -> return Nothing
         
findCredentialByName :: T.Text -> Handler App Sqlite (Maybe Credential)
findCredentialByName cred = do
    r <- query "SELECT cod_credencial FROM tb_credencial WHERE credencial = ?" (Only cred)
    case r of
         [Only credCode :: Only Int64] -> return $ Just $ Credential (Just credCode) cred
         _            -> return Nothing


listCredentialsByUserId :: Int64 -> Handler App Sqlite [Credential]
listCredentialsByUserId userId = do
    r <- query "SELECT cod_credencial, credencial FROM tb_credencial WHERE cod_usuario = ?" (Only userId)
    case r of
         (credentials@(_:_) :: [(Int64, T.Text)]) -> return $ fmap (uncurry Credential . over _1 Just) credentials
         _ -> return []

findUserById :: Int64 -> Handler App Sqlite (Maybe User)
findUserById userId = do 
    r <- query "SELECT login, password FROM tb_usuario WHERE cod_usuario = ?" (Only userId)
    credentials <- listCredentialsByUserId userId
    case r of
        [(login, pw) :: (T.Text, T.Text)] -> return $ Just (User (Just userId) login pw credentials)
        _ -> return Nothing

findUserByCredentialId :: Int64 -> Handler App Sqlite (Maybe User)
findUserByCredentialId credCode = do
    [(Only code) :: (Only Int64)] <- query "SELECT cod_usuario FROM tb_credencial WHERE cod_credencial = ?"
                                        (Only credCode)
    findUserById code

listUsersByContractId :: Int64 -> Handler App Sqlite [User]
listUsersByContractId contrId = do
    r <- query "SELECT cod_usuario FROM tb_usuario WHERE cod_contrato = ?" (Only contrId)
    case r of
        (usersCodes@(_:_) :: [[Int64]]) -> liftM catMaybes $ mapM findUserById (concat usersCodes)
        _ -> return []

findContractById :: Int64 -> Handler App Sqlite (Maybe Contract)
findContractById contrId = do 
    r <- query "SELECT cod_contrato FROM tb_contrato WHERE cod_contrato = ?" (Only contrId)
    users <- listUsersByContractId contrId
    services <- listServicesByContractId contrId
    case r of
        [Only _ :: Only Int64] -> return $ Just (Contract (Just contrId) users services)
        _ -> return Nothing

findContractByUserId :: Int64 -> Handler App Sqlite (Maybe Contract)
findContractByUserId userId = do 
    r <- query "SELECT cod_contrato FROM tb_usuario WHERE cod_usuario = ?" (Only userId)
    case r of
        [Only contrId :: Only Int64] -> findContractById contrId
        _ -> return Nothing

findServiceById :: Int64 -> Handler App Sqlite (Maybe Service)
findServiceById servId = do
    r <- query "SELECT url_servico FROM tb_servico WHERE cod_servico = ?" (Only servId)
    case r of
        [Only url :: Only String] -> return $ Service (Just servId) <$> parseURI url
        _ -> return Nothing


listServicesByContractId :: Int64 -> Handler App Sqlite [Service]
listServicesByContractId contrId = do
    r <- query "SELECT cod_servico FROM tb_contrato_servico WHERE cod_contrato = ?" (Only contrId)
    case r of
        (servicesCodes@(_:_) :: [[Int64]]) -> liftM catMaybes $ mapM findServiceById (concat servicesCodes)
        _ -> return []

type ContractCode' = Int64
type Credential'   = ByteString
type Service'      = URI

serviceExists :: Service' -> Handler App Sqlite Bool
serviceExists service = do
    let queryStr = T.concat . L.intersperse " " $
                            [ "SELECT cod_servico FROM tb_servico"
                            , "WHERE url_servico = ?" ]
    (res :: [Only Int64])  <- query (S.Query queryStr) (Only $ show service)
    return $ not (null res)


canAccessService :: ContractCode' -> Credential' -> Service' -> Handler App Sqlite Bool
canAccessService contract credential service = do
    let queryStr = T.concat . L.intersperse " " $
                            [ "SELECT COUNT(*) FROM tb_servico s"
                            , "INNER JOIN tb_contrato_servico cs"
                            , "INNER JOIN tb_servico_credencial sc"
                            , "ON s.cod_servico = cs.cod_servico AND sc.cod_contrato_servico = cs.cod_contrato_servico"
                            , "AND cs.cod_contrato = ? AND sc.credencial_auth LIKE ?" 
                            , "AND s.url_servico = ? AND sc.datetime_exp > datetime('now')" ]
    [Only status :: Only Int64] <- query (S.Query queryStr) (contract, credential, show service)
    return $ status > 0

