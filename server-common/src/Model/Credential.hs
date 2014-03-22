{-# LANGUAGE OverloadedStrings #-}

module Model.Credential where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.ByteString     (ByteString)
import           Data.Int            (Int64)
import           Data.Time           (UTCTime)

data Credential = Credential {
                code           :: Int64,
                value          :: ByteString,
                creationDate   :: UTCTime,
                expirationDate :: UTCTime
                } deriving (Eq, Show)

instance FromJSON Credential where
        parseJSON (Object v) =
            Credential <$> v .: "code"
                       <*> v .: "credential"
                       <*> v .: "creationDate"
                       <*> v .: "expirationDate"
        parseJSON _ = mzero

instance ToJSON Credential where
        toJSON (Credential c v cd e) =
            object [ "code"           .= c
                   , "credential"     .= v
                   , "creationDate"   .= cd
                   , "expirationDate" .= e
                   ]
