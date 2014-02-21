{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Db where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Int
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

data User = User
          { userCode        :: Maybe Int64
          , userLogin       :: T.Text
          , userPassword    :: T.Text
          , userCredentials :: [Credential]
          } deriving (Eq, Show)

data Contract = Contract
              { contrCode  :: Maybe Int64 
              , contrUsers :: [User]
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

















