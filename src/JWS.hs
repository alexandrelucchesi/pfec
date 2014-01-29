{-# LANGUAGE OverloadedStrings, PackageImports #-}

module JWS where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T

import qualified Crypto.Cipher.AES as K
import qualified Crypto.MAC.HMAC as K
import qualified Crypto.Hash.SHA256 as K
import qualified Crypto.Padding as K
import qualified Crypto.PubKey.HashDescr as K
import qualified Crypto.PubKey.RSA as K
import qualified Crypto.PubKey.RSA.PKCS15 as K
import qualified "crypto-random" Crypto.Random as K

import qualified Base64 as B64
import Util

signJWE :: K.PrivateKey -> B.ByteString -> B.ByteString
signJWE privKey payload =
    let msg = K.sign Nothing K.hashDescrSHA256 privKey payload
    in either (error . show) B64.encode msg

verifyJWE :: K.PublicKey -> B.ByteString -> Bool
verifyJWE pubKey msg =
    let parts = C.split '.' msg
    in if length parts == 3
          then let header  = parts !!0
                   payload = parts !!1
                   sig'    = B64.decode $ parts !!2
                   msg'    = C.intercalate "." [header, payload]
               in K.verify K.hashDescrSHA256 pubKey msg' sig' 
          else error "Signature must have 3 parts."

test :: IO ()
test = do
    g <- cprg
    let ((pubKey, privKey), g') = K.generate g 256 0x10001
        payload = B64.encode "Live long and prosper."
        sigInp  = C.intercalate "." [jwsHeader, payload] -- sigInp is already in ASCII due to nature of ByteStrings (8-bit each elem).
        sig = signJWE privKey sigInp
        res = C.intercalate "." [sigInp, sig]
        status = verifyJWE pubKey res
    putStrLn "Is signature valid?"
    putStrLn $ "-- " ++ show status

