{-# LANGUAGE OverloadedStrings #-} 

module Util.JSONWebToken where

import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified JWT
import qualified Util.Base64 as B64

------------------------------------------------------------------------------ | Converts value to JWT encoded.
toJWT :: ToJSON a => a -> C.ByteString
toJWT = CL.toStrict . B64.encode . encode

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

