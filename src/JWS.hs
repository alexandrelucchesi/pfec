{-# LANGUAGE OverloadedStrings, PackageImports #-}

module JWS where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Crypto.PubKey.HashDescr as K
import qualified Crypto.PubKey.RSA as K
import qualified Crypto.PubKey.RSA.PKCS15 as K

import qualified Base64 as B64
import Util

signJWS :: K.PrivateKey -> T.Text -> B.ByteString
signJWS privKey msg =
    let payload = B64.encode . T.encodeUtf8 $ msg
        sigInp  = C.intercalate "." [jwsHeader, payload] -- sigInp is already in ASCII due to nature of ByteStrings (8-bit each elem).
        signature = K.sign Nothing K.hashDescrSHA256 privKey sigInp
        sig = either (error . show) B64.encode signature
    in C.intercalate "." [sigInp, sig]

verifyJWS :: K.PublicKey -> B.ByteString -> Either String T.Text
verifyJWS pubKey msg =
    let parts = C.split '.' msg
    in if length parts == 3
          then let header  = parts !!0
                   payload = parts !!1
                   sig'    = B64.decode $ parts !!2
                   msg'    = C.intercalate "." [header, payload]
               in if K.verify K.hashDescrSHA256 pubKey msg' sig'
                     then Right (T.decodeUtf8 . B64.decode $ payload)
                     else Left "Signature could not be verified."
          else Left "Signature must have 3 parts."

test :: IO ()
test = do
    g <- cprg
    let ((pubKey, privKey), _) = K.generate g 256 0x10001
        msg = "Live long and prosper."
        res = signJWS privKey msg
        status = verifyJWS pubKey res
    putStrLn "Is signature valid?"
    putStrLn $ "-- " ++ show status

