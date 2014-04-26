{-# LANGUAGE OverloadedStrings #-} 

module Util.JSONWebToken where

import Control.Applicative
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified JWT
import qualified JWE
import qualified JWS
import qualified Util.Base64 as B64
import Model.PublicKey
    ( PublicKey(..)
    , rsaKey
    )
import Crypto.PubKey.RSA
    ( PrivateKey
    )

------------------------------------------------------------------------------ | Converts value to JWT encoded.
toB64JSON :: ToJSON a => a -> IO C.ByteString
toB64JSON = return . B64.encode . encode

fromB64JSON :: FromJSON a => C.ByteString -> IO (Maybe a)
fromB64JSON = return . decode . B64.decode'

--toCompactJWT :: ToJSON a => a -> IO C.ByteString
--toCompactJWT jwtContents = do
--    myPrivKey   <- liftM read $ readFile "../server-common/resources/keys/rsa-key.priv"
--    theirPubKey <- liftM read $ readFile "../server-common/resources/keys/TJDFT/rsa-key.pub"
--    JWT.toCompact myPrivKey theirPubKey jwtContents
--
--fromCompactJWT :: FromJSON a => C.ByteString -> IO (Maybe a)
--fromCompactJWT jwtContents = do
--    myPrivKey   <- liftM read $ readFile "../server-common/resources/keys/rsa-key.priv"
--    theirPubKey <- liftM read $ readFile "../server-common/resources/keys/TJDFT/rsa-key.pub"
--    JWT.fromCompact myPrivKey theirPubKey jwtContents

serverPrivKey :: IO PrivateKey
serverPrivKey = liftM read $ readFile --"../server-common/resources/keys/rsa-key.priv"
    "/Users/alexandrelucchesi/Development/haskell/pfec/jwt-min/data/keys/rsa/rec_key.priv"

serverPubKey :: IO PublicKey
serverPubKey = liftM (RSAPublicKey . read) $ readFile --"../server-common/resources/keys/rsa-key.pub"
    "/Users/alexandrelucchesi/Development/haskell/pfec/jwt-min/data/keys/rsa/rec_key.pub"

-- Returns a value and the message whose signature must be verified.
decrypt :: FromJSON a => PrivateKey -> C.ByteString -> Maybe (a, C.ByteString)
decrypt privKey jweContents =
    -- TODO: Handler header properly. I'm ignoring it because we're using
    -- only one algorithm.
    let (_, jwsContents) = JWE.decryptJWE privKey jweContents
    in case C.split '.' jwsContents of
           (_:msg:_) -> (,) <$> (decode . B64.decode') msg
                            <*> pure jwsContents
           _ -> Nothing

-- Verifies signature using sender's public key. It must be taken from
-- Server's database using the contract UUID provided (known after
-- decryption).
verify :: PublicKey -> C.ByteString -> Bool
verify pubKey jwsContents =
    let eitherMsg = JWS.verifyJWS (rsaKey pubKey) jwsContents
    in case eitherMsg of
           Right _ -> True
           _       -> False

signAndEncrypt :: ToJSON a => PrivateKey -> PublicKey -> a -> IO C.ByteString
signAndEncrypt privKey pubKey = JWT.toCompact privKey (rsaKey pubKey)


