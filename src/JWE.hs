{-# LANGUAGE OverloadedStrings, PackageImports #-}

module JWE where

import qualified Data.ByteString as B
import qualified Data.ByteString as C
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Crypto.Cipher.AES as K
import qualified Crypto.MAC.HMAC as K
import qualified Crypto.Hash.SHA256 as K
import qualified Crypto.Padding as K
import qualified Crypto.PubKey.RSA as K
import qualified Crypto.PubKey.RSA.PKCS15 as K
import qualified "crypto-random" Crypto.Random as K

import qualified Codec.Encryption.Padding as K

import qualified Base64 as B64

message :: T.Text
message = "Live long and prosper."

-----------------------------------------------------------------------------------------------------------------------
-- ENCODING
-----------------------------------------------------------------------------------------------------------------------
jweHeader :: B.ByteString
jweHeader = B64.encode . T.encodeUtf8 $ "{\"alg\":\"RSA1_5\",\"enc\":\"A128CBC-HS256\"}"

cek :: (K.CPRG c) => c -> (B.ByteString, c)
cek cprg' = K.cprgGenerate 32 cprg'

iv :: (K.CPRG c) => c -> (B.ByteString, c)
iv cprg' = K.cprgGenerate 16 cprg'

cprg :: IO K.SystemRNG
cprg = K.createEntropyPool >>= return . K.cprgCreate

jweEncryptedKey :: (K.CPRG c) => c -> K.PublicKey -> (B.ByteString, c)
jweEncryptedKey cprg' pubKey =
    let (cek', cprg'') = cek cprg'
        (res, cprg''') = K.encrypt cprg'' pubKey cek'
        key = either (error . show) B64.encode $ res 
    in (key, cprg''')

encrypt_AES_128_CBC_HMAC_SHA_256 :: B.ByteString -> B.ByteString -> B.ByteString -> (B.ByteString, B.ByteString)
encrypt_AES_128_CBC_HMAC_SHA_256 key iv' plaintext =
    let mac_key = B.take 16 key
        enc_key = B.take 16 (B.reverse key)
        aesCtx  = K.initAES enc_key
        cypher  = K.encryptCBC aesCtx iv' plaintext
        hmacInp = iv' `B.append` cypher
        authTag = K.hmac K.hash 16 mac_key hmacInp
    in (B64.encode cypher, B64.encode authTag)

encryptJwe :: (K.CPRG c) => c -> K.PublicKey -> T.Text -> B.ByteString
encryptJwe g pubKey plaintext =
    let (key, g')         = jweEncryptedKey g pubKey
        (cek', g'')       = cek g'
        (iv', _)          = iv g''
        (cypher, authTag) = encrypt_AES_128_CBC_HMAC_SHA_256 cek' iv' (K.padPKCS5 16 . T.encodeUtf8 $ plaintext)
        res = [jweHeader, key, iv', cypher, authTag] 
    in B.intercalate "." res

-----------------------------------------------------------------------------------------------------------------------
-- DECODING
-----------------------------------------------------------------------------------------------------------------------
-- TODO

test :: IO ()
test = do
    g <- cprg
--    either print (print . B.length . fst) $ K.pad g 2 (T.encodeUtf8 message)
    let ((pubKey, privKey), g') = K.generate g 256 0x10001
    print $ encryptJwe g' pubKey message
    












