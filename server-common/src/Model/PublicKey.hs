{-# LANGUAGE OverloadedStrings #-}

module Model.PublicKey where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson

import qualified Crypto.PubKey.RSA as RSA (PublicKey)

data PublicKey = RSAPublicKey {
               rsaKey :: RSA.PublicKey
               } deriving (Eq, Read, Show)

instance FromJSON PublicKey where
        parseJSON (Object v) =
            RSAPublicKey . read <$> v .: "rsaKey"
        parseJSON _ = mzero

instance ToJSON PublicKey where
        toJSON (RSAPublicKey k) =
            object [ "rsaKey" .= show k ]

