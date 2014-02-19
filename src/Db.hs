{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Db where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Int
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


findCredentialById :: Int64 -> Handler App Sqlite (Maybe Credential)
findCredentialById credCode = do
    r <- query "SELECT credencial FROM tb_credencial WHERE cod_credencial = ?" (Only credCode)
    case r of
         [Only cred :: Only T.Text] -> return $ Just $ Credential (Just credCode) cred
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


