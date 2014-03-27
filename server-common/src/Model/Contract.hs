{-# LANGUAGE OverloadedStrings #-}

module Model.Contract where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.ByteString           (ByteString)
import           Data.Text                 (Text)
import           Data.Time                 (UTCTime)

import           Model.User
import           Model.Service
import           Model.Credential
import           Model.Challenge
import           Model.PublicKey
import           Model.Token
import           Model.UUID

data Contract = Contract {
              uuid                 :: UUID,       -- CouchDB's.
              revision             :: ByteString, -- CouchDB's.
              name                 :: Text,
              description          :: Text,
              creationDate         :: UTCTime,
              users                :: [User],
              services             :: [Service],
              credentials          :: [Credential],
              challengesCredential :: [ChallengeCredential],
              challengesAuth       :: [ChallengeAuth],
              publicKeys           :: [PublicKey],
              tokens               :: [Token]
              } deriving (Eq, Show)
              
instance FromJSON Contract where
    parseJSON (Object v) =
        Contract <$> v .: "_id"  -- CouchDB's.
                 <*> v .: "_rev" -- CouchDB's.
                 <*> v .: "name"
                 <*> v .: "description"
                 <*> v .: "creationDate"
                 <*> v .: "users"
                 <*> v .: "services"
                 <*> v .: "credentials"
                 <*> v .: "challengesCredential"
                 <*> v .: "challengesAuth"
                 <*> v .: "publicKeys"
                 <*> v .: "tokens"
    parseJSON _ = mzero

instance ToJSON Contract where
    toJSON (Contract _ _ n d cd u s cr cc ca p t) =
        object [ "name"                 .= n
               , "description"          .= d
               , "creationDate"         .= cd
               , "users"                .= u
               , "services"             .= s
               , "credentials"          .= cr
               , "challengesCredential" .= cc
               , "challengesAuth"       .= ca
               , "publicKeys"           .= p
               , "tokens"               .= t
               ]

