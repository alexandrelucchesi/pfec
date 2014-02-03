{-# LANGUAGE OverloadedStrings, PackageImports #-}

module JWT where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Control.Monad
import qualified Crypto.Padding as K
import qualified Crypto.PubKey.RSA as K
import qualified "crypto-random" Crypto.Random as K
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T

import qualified JWE as E
import qualified JWS as S
import Util

-- NOTE: This function is identical to "encryptJWE". The only difference is that it uses "jwtHeader" instead of "jweHeader".
-- TODO: Improve the design.
encryptJWT :: (K.CPRG c) => c -> K.PublicKey -> T.Text -> B.ByteString
encryptJWT g pubKey plaintext =
    let (cek', g')        = cek g -- randomly generate a content encryption key.
        (key, g'')        = E.jweEncryptedKey g' pubKey cek' -- encrypt CEK using the recipient's public key.
        (iv', _)          = iv g'' -- randomly generate an Initialization Vector.
        (cipher, authTag) = E.encrypt_AES_128_CBC_HMAC_SHA_256 cek' iv' (K.padPKCS5 16 . T.encodeUtf8 $ plaintext) -- encrypt the plaintext using PKCS #5 for padding.
        res = [jwtHeader, key, iv', cipher, authTag] 
    in
        B.intercalate "." res

test :: IO ()
test = do
    -- ENCODE COMPACT JWT
    myPrivKey   <- liftM read $ readFile "data/keys/rsa/sen_key.priv"
    theirPubKey <- liftM read $ readFile "data/keys/rsa/rec_key.pub"
    g <- cprg

    let msg = "Haskell rulez!"
        jwe = S.signJWS myPrivKey msg
        jwt = encryptJWT g theirPubKey (T.decodeUtf8 jwe) 

    C.putStrLn jwt

    -- DECODE COMPACT JWT
    myPubKey     <- liftM read $ readFile "data/keys/rsa/sen_key.pub"
    theirPrivKey <- liftM read $ readFile "data/keys/rsa/rec_key.priv"

    let (header, jwe') = E.decryptJWE theirPrivKey jwt 
        (Right msg')    = S.verifyJWS myPubKey (T.encodeUtf8 jwe')

    C.putStrLn header
    T.putStrLn msg'

