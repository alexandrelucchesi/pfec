{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

module JWE where

import qualified Crypto.Cipher.AES        as K
import qualified Crypto.Hash.SHA256       as K
import qualified Crypto.MAC.HMAC          as K
import qualified Crypto.Padding           as K
import qualified Crypto.PubKey.RSA        as K
import qualified Crypto.PubKey.RSA.PKCS15 as K
import qualified "crypto-random" Crypto.Random            as K
import qualified Data.ByteString          as B
import qualified Data.ByteString.Char8    as C
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Text.IO             as T

import qualified Base64                   as B64
import           Util

-----------------------------------------------------------------------------------------------------------------------
-- ENCODING
-----------------------------------------------------------------------------------------------------------------------
jweEncryptedKey :: (K.CPRG c) => c -> K.PublicKey -> B.ByteString -> (B.ByteString, c)
jweEncryptedKey cprg' pubKey cek' =
    let (res, cprg'') = K.encrypt cprg' pubKey cek'
        --key = either (error . show) B64.encode res
        key = either (error . show) id res
    in (key, cprg'')

encrypt_AES_128_CBC_HMAC_SHA_256 :: B.ByteString -> B.ByteString -> B.ByteString -> (B.ByteString, B.ByteString)
encrypt_AES_128_CBC_HMAC_SHA_256 key iv' plaintext =
    let macKey  = B.take 16 key -- first 16 bytes is the MAC key.
        encKey  = B.reverse . B.take 16 . B.reverse $ key -- last 16 bytes is the Encryption key.
        aesCtx  = K.initAES encKey
        cipher  = K.encryptCBC aesCtx iv' plaintext -- encrypt using Cypher Block Chaining mode.
        hmacInp = iv' `B.append` cipher
        authTag = B.take 16 $ K.hmac K.hash 16 macKey hmacInp -- computes Mac Authentication Code using SHA-256 algorithm.
    --in (B64.encode cipher, B64.encode authTag)
    in (cipher, authTag)

encrypt_RSA :: (K.CPRG c) => c -> K.PublicKey -> B.ByteString -> (B.ByteString, c)
encrypt_RSA g pubKey msg =
    let (res, g') = K.encrypt g pubKey msg
        res' = either (error . show) B64.encode $ res
    in (res', g')

encryptJWE :: (K.CPRG c) => c -> K.PublicKey -> C.ByteString -> C.ByteString
encryptJWE g pubKey plaintext =
    let (cek', g')        = cek g -- randomly generate a content encryption key.
        (key, g'')        = jweEncryptedKey g' pubKey cek' -- encrypt CEK using the recipient's public key.
        (iv', _)          = iv g'' -- randomly generate an Initialization Vector.
        (cipher, authTag) = encrypt_AES_128_CBC_HMAC_SHA_256 cek' iv' (K.padPKCS5 16 $ plaintext) -- encrypt the plaintext using PKCS #5 for padding.
        res = map B64.encode [jweHeader, key, iv', cipher, authTag]
    in
        B.intercalate "." res

-----------------------------------------------------------------------------------------------------------------------
-- DECODING
-----------------------------------------------------------------------------------------------------------------------
decodeJweHeader :: B.ByteString -> C.ByteString
decodeJweHeader = B64.decode

jweDecryptedKey :: K.PrivateKey -> B.ByteString -> B.ByteString
jweDecryptedKey privKey encKey =
    let key = K.decrypt Nothing privKey encKey
    in either (error . show) id key

decrypt_AES_128_CBC_HMAC_SHA_256 :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
decrypt_AES_128_CBC_HMAC_SHA_256 key iv' ciphertxt authTag =
    let mac_key  = B.take 16 key
        enc_key  = B.reverse . B.take 16 . B.reverse $ key
        aesCtx   = K.initAES enc_key
        msg      = K.unpadPKCS5 $ K.decryptCBC aesCtx iv' ciphertxt
        hmacInp  = iv' `B.append` ciphertxt
        authTag' = B.take 16 $ K.hmac K.hash 16 mac_key hmacInp
    in if authTag' == authTag
          then msg
          else error $ "Decrypt: Auth Tag: " ++ show authTag ++
                  "\nAuth Tag': " ++ show authTag' ++
                  "\nMessage: " ++ show msg

decrypt_RSA :: K.PrivateKey -> B.ByteString -> T.Text
decrypt_RSA privKey ciphertxt =
    let ciphertxt' = B64.decode ciphertxt
        msg = K.decrypt Nothing privKey ciphertxt'
    in either (error . show) (T.decodeUtf8) msg

decryptJWE :: K.PrivateKey -> B.ByteString -> (C.ByteString, C.ByteString)
decryptJWE privKey ciphertxt =
    let parts = C.split '.' ciphertxt
    in if length parts /= 5
          then error "Invalid content."
          else let parts'  = map B64.decode parts
                   header  = parts' !! 0
                   encKey  = parts' !! 1
                   iv'     = parts' !! 2
                   encMsg  = parts' !! 3
                   authTag = parts' !! 4
                   cek'    = jweDecryptedKey privKey encKey
                   msg     = decrypt_AES_128_CBC_HMAC_SHA_256 cek' iv' encMsg authTag
               in
                   (header, msg)

test_AES_128_CBC_HMAC_SHA_256 :: IO ()
test_AES_128_CBC_HMAC_SHA_256 = do
    g <- cprg
    let ((pubKey, privKey), g') = K.generate g 256 0x10001
        message = "Live long and prosper."
        cipher = encryptJWE g' pubKey message
        origin = decryptJWE privKey cipher
    print $ C.split '.' cipher
   -- C.putStrLn $ fst origin
    C.putStrLn $ snd origin

test_RSA :: IO ()
test_RSA = do
    g <- cprg
    let ((pubKey, privKey), g') = K.generate g 256 0x10001
        message = "Live long and prosper."
        (cipher,_) = encrypt_RSA g' pubKey message
        origin = decrypt_RSA privKey cipher
    T.putStrLn $ origin



