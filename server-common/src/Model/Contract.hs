{-# LANGUAGE OverloadedStrings #-}

module Model.Contract where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Text                 (Text)
import           Data.Time                 (UTCTime)

import           Model.User
import           Model.Service
import           Model.Credential
import           Model.Challenge
import           Model.PublicKey
import           Model.UUID

data Contract = Contract {
              uuid                 :: UUID,
              name                 :: Text,
              description          :: Text,
              creationDate         :: UTCTime,
              users                :: [User],
              services             :: [Service],
              credentials          :: [Credential],
              challengesCredential :: [ChallengeCredential],
              challengesAuth       :: [ChallengeAuth],
              publicKeys           :: [PublicKey]
              } deriving (Eq, Show)
              
instance FromJSON Contract where
    parseJSON (Object v) =
        Contract <$> v .: "uuid"
                 <*> v .: "name"
                 <*> v .: "description"
                 <*> v .: "creationDate"
                 <*> v .: "users"
                 <*> v .: "services"
                 <*> v .: "credentials"
                 <*> v .: "challengesCredential"
                 <*> v .: "challengesAuth"
                 <*> v .: "publicKeys"
    parseJSON _ = mzero

instance ToJSON Contract where
    toJSON (Contract c n d cd u s cr cc ca p) =
        object [ "uuid"                 .= c
               , "name"                 .= n
               , "description"          .= d
               , "creationDate"         .= cd
               , "users"                .= u
               , "services"             .= s
               , "credentials"          .= cr
               , "challengesCredential" .= cc
               , "challengesAuth"       .= ca
               , "publicKeys"           .= p
               ]

