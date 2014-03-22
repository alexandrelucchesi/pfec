{-# LANGUAGE OverloadedStrings #-} 

module Util.JSONWebToken where

import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Char8 as C
import qualified JWT
import qualified Util.Base64 as B64

------------------------------------------------------------------------------ | Converts value to JWT encoded.
toB64JSON :: ToJSON a => a -> IO C.ByteString
toB64JSON = return . B64.encode . encode

fromB64JSON :: FromJSON a => C.ByteString -> IO (Maybe a)
fromB64JSON = return . decode . B64.decode'

toCompactJWT :: ToJSON a => a -> IO C.ByteString
toCompactJWT jwtContents = do
    myPrivKey   <- liftM read $ readFile "../server-common/resources/keys/rsa-key.priv"
    theirPubKey <- liftM read $ readFile "../server-common/resources/keys/TJDFT/rsa-key.pub"
    JWT.toCompact myPrivKey theirPubKey jwtContents

fromCompactJWT :: FromJSON a => C.ByteString -> IO (Maybe a)
fromCompactJWT jwtContents = do
    myPrivKey   <- liftM read $ readFile "../server-common/resources/keys/rsa-key.priv"
    theirPubKey <- liftM read $ readFile "../server-common/resources/keys/TJDFT/rsa-key.pub"
    JWT.fromCompact myPrivKey theirPubKey jwtContents

