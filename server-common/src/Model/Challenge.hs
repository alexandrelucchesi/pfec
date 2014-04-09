{-# LANGUAGE OverloadedStrings #-}

module Model.Challenge where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.ByteString     (ByteString)
import           Data.Time           (UTCTime)

import           Model.UUID (UUID(..))

data Challenge = Challenge {
               uuid           :: UUID,
               answer         :: ByteString,
               expirationDate :: UTCTime,
               wasAnswered    :: Bool
               } deriving (Eq, Show)

instance FromJSON Challenge where
    parseJSON (Object v) =
        Challenge <$> v .: "_id"
                  <*> v .: "answer"
                  <*> v .: "expirationDate"
                  <*> v .: "wasAnswered"
    parseJSON _ = mzero

instance ToJSON Challenge where
    toJSON (Challenge _ a ed wa) =
        object [ "type"           .= ("challenge" :: ByteString)
               , "answer"         .= a
               , "expirationDate" .= ed
               , "wasAnswered"    .= wa
               ]
