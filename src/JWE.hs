{-# LANGUAGE OverloadedStrings, PackageImports #-}

module JWE where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T

import qualified Crypto.Cipher.AES as K
import qualified Crypto.MAC.HMAC as K
import qualified Crypto.Hash.SHA256 as K
import qualified Crypto.Padding as K
import qualified Crypto.PubKey.RSA as K
import qualified Crypto.PubKey.RSA.PKCS15 as K
import qualified "crypto-random" Crypto.Random as K

import qualified Base64 as B64
import Util

-----------------------------------------------------------------------------------------------------------------------
-- ENCODING
-----------------------------------------------------------------------------------------------------------------------
jweEncryptedKey :: (K.CPRG c) => c -> K.PublicKey -> B.ByteString -> (B.ByteString, c)
jweEncryptedKey cprg' pubKey cek' =
    let (res, cprg'') = K.encrypt cprg' pubKey cek'
        key = either (error . show) B64.encode $ res 
    in (key, cprg'')

encrypt_AES_128_CBC_HMAC_SHA_256 :: B.ByteString -> B.ByteString -> B.ByteString -> (B.ByteString, B.ByteString)
encrypt_AES_128_CBC_HMAC_SHA_256 key iv' plaintext =
    let macKey  = B.take 16 key -- first 16 bytes is the MAC key.
        encKey  = B.take 16 (B.reverse key) -- last 16 bytes is the Encryption key.
        aesCtx  = K.initAES encKey
        cipher  = K.encryptCBC aesCtx iv' plaintext -- encrypt using Cypher Block Chaining mode.
        hmacInp = iv' `B.append` cipher
        authTag = K.hmac K.hash 16 macKey hmacInp -- computes Mac Authentication Code using SHA-256 algorithm.
    in (B64.encode cipher, B64.encode authTag)

encryptJWE :: (K.CPRG c) => c -> K.PublicKey -> T.Text -> B.ByteString
encryptJWE g pubKey plaintext =
    let (cek', g')        = cek g -- randomly generate a content encryption key.
        (key, g'')        = jweEncryptedKey g' pubKey cek' -- encrypt CEK using the recipient's public key.
        (iv', _)          = iv g'' -- randomly generate an Initialization Vector.
        (cipher, authTag) = encrypt_AES_128_CBC_HMAC_SHA_256 cek' iv' (K.padPKCS5 16 . T.encodeUtf8 $ plaintext) -- encrypt the plaintext using PKCS #5 for padding.
        res = [jweHeader, key, iv', cipher, authTag] 
    in
        B.intercalate "." res

-----------------------------------------------------------------------------------------------------------------------
-- DECODING
-----------------------------------------------------------------------------------------------------------------------
decodeJweHeader :: B.ByteString -> C.ByteString
decodeJweHeader encHeader = B64.decode encHeader

jweDecryptedKey :: K.PrivateKey -> B.ByteString -> B.ByteString
jweDecryptedKey privKey encKey =
    let encKey' = B64.decode encKey
        key = K.decrypt Nothing privKey encKey'
    in either (error . show) id key
 
decrypt_AES_128_CBC_HMAC_SHA_256 :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString -> T.Text
decrypt_AES_128_CBC_HMAC_SHA_256 key iv' ciphertxt authTag =
    let mac_key  = B.take 16 key
        enc_key  = B.take 16 (B.reverse key)
        aesCtx   = K.initAES enc_key
        msg      = K.unpadPKCS5 $ K.decryptCBC aesCtx iv' ciphertxt
        hmacInp  = iv' `B.append` ciphertxt
        authTag' = K.hmac K.hash 16 mac_key hmacInp
    in if authTag' == authTag
          then T.decodeUtf8 msg
          else error $ "Decrypt: Auth Tag: " ++ show authTag' ++
                  "\nMessage: " ++ show msg

decryptJWE :: K.PrivateKey -> C.ByteString -> (C.ByteString, T.Text)
decryptJWE privKey ciphertxt =
    let parts = C.split '.' ciphertxt
    in if length parts /= 5
          then error "Invalid content."
          else let encHeader  = parts !!0 
                   encKey     = parts !!1 
                   encIV      = parts !!2 
                   encMsg     = B64.decode $ parts !!3 
                   encAuthTag = B64.decode $ parts !!4
                   header     = decodeJweHeader encHeader
                   cek'       = jweDecryptedKey privKey encKey
                   iv'        = encIV
                   msg        = decrypt_AES_128_CBC_HMAC_SHA_256 cek' iv' encMsg encAuthTag
               in
                   (header, msg)

test :: IO ()
test = do
    g <- cprg
    let ((pubKey, privKey), g') = K.generate g 256 0x10001
        message = "Live long and prosper."
        cipher = encryptJWE g' pubKey message
        origin = decryptJWE privKey cipher
    print $ C.split '.' cipher
   -- C.putStrLn $ fst origin
    T.putStrLn $ snd origin
    
