{-# LANGUAGE OverloadedStrings #-}

module Model.Contract where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.ByteString           (ByteString)
import           Data.Text                 (Text)
import           Data.Time                 (UTCTime)

import           Model.Credential
import           Model.PublicKey
import           Model.UUID

data Contract = Contract {
              uuid                 :: UUID,
              revision             :: ByteString, -- Necessary for updates.
              name                 :: Text,
              description          :: Text,
              creationDate         :: UTCTime,
              credentials          :: [Credential],
              publicKeys           :: [PublicKey]
              } deriving (Eq, Show)
              
instance FromJSON Contract where
    parseJSON (Object v) =
        Contract <$> v .: "_id"
                 <*> v .: "_rev" -- Necessary for updates.
                 <*> v .: "name"
                 <*> v .: "description"
                 <*> v .: "creationDate"
                 <*> v .: "credentials"
                 <*> v .: "publicKeys"
    parseJSON _ = mzero

instance ToJSON Contract where
    toJSON (Contract _ _ n d cd c pk) =
        object [ "type"         .= ("contract" :: ByteString)
               , "name"         .= n
               , "description"  .= d
               , "creationDate" .= cd
               , "credentials"  .= c
               , "publicKeys"   .= pk
               ]

