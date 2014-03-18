{-# LANGUAGE OverloadedStrings #-}

module Model where

import           Data.ByteString           (ByteString)
import           Data.Int                  (Int64)
import           Data.Text                 (Text)
import           Data.Time                 (UTCTime)
import           Data.Word                 (Word16)
import           Network.HTTP.Types.Method (Method)

import           Messages.Types

data Contract = Contract {
              contractCode                 :: Int64,
              contractName                 :: Text,
              contractDescription          :: Text,
              contractCreationDate         :: UTCTime,
              contractUsers                :: [User],
              contractServices             :: [Service],
              contractCredentials          :: [Credential],
              contractCredentialChallenges :: [ChallengeCredential],
              contractAuthChallenges       :: [ChallengeAuth]
              } deriving (Eq, Show)

data User = User {
          userCode     :: Int64,
          userLogin    :: ByteString,
          userPassword :: ByteString
          } deriving (Eq, Show)

data Service = Service {
             serviceHost    :: ByteString,
             servicePort    :: Word16,
             servicePath    :: ByteString,
             serviceURL     :: URI,
             serviceMethods :: [Method],
             serviceTokens  :: [Token]
             } deriving (Eq, Show)

data Token = Token {
           tokenValue          :: ByteString,
           tokenCreationDate   :: UTCTime,
           tokenExpirationDate :: UTCTime
           } deriving (Eq, Show)

data Credential = Credential {
                credentialValue          :: ByteString,
                credentialCreationDate   :: UTCTime,
                credentialExpirationDate :: UTCTime
                } deriving (Eq, Show)

data Challenge = Challenge {
               challengeAnswer       :: ByteString,
               challengeCreationDate :: UTCTime
               } deriving (Eq, Show)

-- Credential challenge (resolved in Facade Server).
type ChallengeCredential = Challenge
-- Authentication and authorization challenge (resolved in Auth Server).
type ChallengeAuth       = Challenge

