{-# LANGUAGE OverloadedStrings #-}

module Model.Contract where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.ByteString     (ByteString)
import           Data.Text           (Text)
import           Data.Time           (UTCTime)

import           Model.Credential    (Credential)
import           Model.PublicKey     (PublicKey)
import           Model.UUID          (UUID)

type ServiceUUID = UUID
data Contract = Contract {
              uuid                 :: UUID,
              revision             :: ByteString, -- Necessary for updates.
              name                 :: Text,
              description          :: Text,
              creationDate         :: UTCTime,
              credentials          :: [Credential], -- TODO: Change to HashMap.
              allowedServicesUUIDs :: [ServiceUUID],
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
                 <*> v .: "allowedServicesUUIDs"
                 <*> v .: "publicKeys"
    parseJSON _ = mzero

instance ToJSON Contract where
    toJSON (Contract _ _ n d cd c as pk) =
        object [ "type"                 .= ("contract" :: ByteString)
               , "name"                 .= n
               , "description"          .= d
               , "creationDate"         .= cd
               , "credentials"          .= c
               , "allowedServicesUUIDs" .= as
               , "publicKeys"           .= pk
               ]

