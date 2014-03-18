{-# LANGUAGE OverloadedStrings #-}

module Model.User where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.ByteString     (ByteString)
import           Data.Int            (Int64)

data User = User {
          code     :: Int64,
          login    :: ByteString,
          password :: ByteString
          } deriving (Eq, Show)

instance FromJSON User where
        parseJSON (Object v) =
            User <$> v .: "code"
                 <*> v .: "login"
                 <*> v .: "password"
        parseJSON _ = mzero

instance ToJSON User where
        toJSON (User userCode userLogin userPassword) =
            object [ "code"     .= userCode,
                     "login"    .= userLogin,
                     "password" .= userPassword ]

