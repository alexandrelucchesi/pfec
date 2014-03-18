{-# LANGUAGE OverloadedStrings #-}

module Model.Challenge where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.ByteString     (ByteString)
import           Data.Time           (UTCTime)

import           Model.UUID

data Challenge = Challenge {
               uuid         :: UUID,
               answer       :: ByteString,
               creationDate :: UTCTime
               } deriving (Eq, Show)

-- Credential challenge (resolved in Facade Server).
type ChallengeCredential = Challenge
-- Authentication and authorization challenge (resolved in Auth Server).
type ChallengeAuth       = Challenge

instance FromJSON Challenge where
    parseJSON (Object v) =
        Challenge <$> v .: "uuid"
                  <*> v .: "answer"
                  <*> v .: "creationDate"
    parseJSON _ = mzero

instance ToJSON Challenge where
    toJSON (Challenge u a c) =
        object [ "uuid"         .= u,
                 "answer"       .= a,
                 "creationDate" .= c ]

