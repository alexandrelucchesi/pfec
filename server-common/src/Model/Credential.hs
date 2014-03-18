{-# LANGUAGE OverloadedStrings #-}

module Model.Credential where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.ByteString           (ByteString)
import           Data.Time                 (UTCTime)

data Credential = Credential {
                value          :: ByteString,
                creationDate   :: UTCTime,
                expirationDate :: UTCTime
                } deriving (Eq, Show)

instance FromJSON Credential where
        parseJSON (Object v) =
            Credential <$> v .: "credential"
                       <*> v .: "creationDate"
                       <*> v .: "expirationDate"
        parseJSON _ = mzero

instance ToJSON Credential where
        toJSON (Credential credentialValue credentialCreationDate credentialExpirationDate) =
            object [ "credential"     .= credentialValue,
                     "creationDate"   .= credentialCreationDate,
                     "expirationDate" .= credentialExpirationDate ]
