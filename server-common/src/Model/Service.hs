{-# LANGUAGE OverloadedStrings #-}

module Model.Service where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.ByteString           (ByteString)
import           Data.Word                 (Word16)
import           Network.HTTP.Types.Method (Method)
--import           Snap.Internal.Http.Types (Method)

import           Model.URI
import           Model.Token

data Service = Service {
             host    :: ByteString,
             port    :: Word16,
             path    :: ByteString,
             methods :: [Method],
             tokens  :: [Token]
             } deriving (Eq, Show)

url :: Service -> URI
url = undefined

instance FromJSON Service where
        parseJSON (Object v) =
            Service <$> v .: "host"
                    <*> v .: "port"
                    <*> v .: "path"
                    <*> v .: "methods"
                    <*> v .: "tokens"
        parseJSON _ = mzero

instance ToJSON Service where
        toJSON (Service serviceHost servicePort servicePath serviceMethods serviceTokens) =
            object [ "host"    .= serviceHost,
                     "port"    .= servicePort,
                     "path"    .= servicePath,
                     "methods" .= serviceMethods,
                     "tokens"  .= serviceTokens ]

