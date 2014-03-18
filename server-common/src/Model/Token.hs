{-# LANGUAGE OverloadedStrings #-}

module Model.Token where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.ByteString           (ByteString)
import           Data.Time                 (UTCTime)

data Token = Token {
           value          :: ByteString,
           creationDate   :: UTCTime,
           expirationDate :: UTCTime
           } deriving (Eq, Show)

instance FromJSON Token where
        parseJSON (Object v) =
            Token <$> v .: "token"
                  <*> v .: "creationDate"
                  <*> v .: "expirationDate"
        parseJSON _ = mzero

instance ToJSON Token where
        toJSON (Token tokenValue tokenCreationDate tokenExpirationDate) =
            object [ "token"     .= tokenValue,
                     "creationDate"    .= tokenCreationDate,
                     "expirationDate" .= tokenExpirationDate ]

