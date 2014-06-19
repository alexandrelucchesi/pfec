{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Credential where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.ByteString     (ByteString)
import           Data.Int            (Int64)

data Credential = Credential {
                code           :: Int64,
                value          :: ByteString
                } deriving (Eq, Show)

instance FromJSON Credential where
        parseJSON (Object v) =
            Credential <$> v .: "code"
                       <*> v .: "credential"
        parseJSON _ = mzero

instance ToJSON Credential where
        toJSON (Credential c cr) =
            object [ "code"           .= c
                   , "credential"     .= cr ]
