{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

module JWT where

import           Control.Monad
import qualified Crypto.Padding             as K
import qualified Crypto.PubKey.RSA          as K
import qualified "crypto-random" Crypto.Random              as K
import           Data.Aeson
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as C
import qualified Data.ByteString.Lazy.Char8 as CL

import qualified JWE                        as E
import qualified JWS                        as S
import           Util
import qualified Base64                     as B64

-- NOTE: This function is identical to "encryptJWE". The only difference is that it uses "jwtHeader" instead of "jweHeader".
-- TODO: Improve the design.
encrypt :: (K.CPRG c) => c -> K.PublicKey -> C.ByteString -> C.ByteString
encrypt g pubKey plaintext =
    let (cek', g')        = cek g -- randomly generate a content encryption key.
        (key, g'')        = E.jweEncryptedKey g' pubKey cek' -- encrypt CEK using the recipient's public key.
        (iv', _)          = iv g'' -- randomly generate an Initialization Vector.
        (cipher, authTag) = E.encrypt_AES_128_CBC_HMAC_SHA_256 cek' iv' (K.padPKCS5 16 $ plaintext) -- encrypt the plaintext using PKCS #5 for padding.
        res = map B64.encode [jwtHeader, key, iv', cipher, authTag]
    in
        B.intercalate "." res

toCompact :: ToJSON a => K.PrivateKey -> K.PublicKey -> a -> IO C.ByteString
toCompact myPrivKey theirPubKey jwtContents = do
    g <- cprg
    let msg = CL.toStrict . encode $ jwtContents
        jwe = S.signJWS myPrivKey msg
    return $ encrypt g theirPubKey jwe

--fromCompact :: FromJSON a => K.PrivateKey -> K.PublicKey -> C.ByteString -> IO (Maybe a)
--fromCompact myPrivKey theirPubKey jwtCompact = do
--    let (header, jwe') = E.decryptJWE myPrivKey jwtCompact
--        (Right msg')   = S.verifyJWS theirPubKey jwe'
--    return . decode' . CL.fromStrict $ msg'
--
--fromCompact' :: K.PrivateKey -> K.PublicKey -> C.ByteString -> IO C.ByteString
--fromCompact' myPrivKey theirPubKey jwtCompact = do
--    let (header, jwe') = E.decryptJWE myPrivKey jwtCompact
--        (Right msg')   = S.verifyJWS theirPubKey jwe'
--    return msg'

test :: IO ()
test = do
    -- ENCODE COMPACT JWT
    myPrivKey   <- liftM read $ readFile "data/keys/rsa/sen_key.priv"
    theirPubKey <- liftM read $ readFile "data/keys/rsa/rec_key.pub"
    g <- cprg

    --let msg = "Haskell rulez!"
    let msg = "{\"contractUUID\":\"3fa46b22-b2ce-479a-b789-97dea0bbb72c\"}"
        jwe = S.signJWS myPrivKey msg
        jwt = encrypt g theirPubKey jwe

    C.putStrLn jwt

    -- DECODE COMPACT JWT
    myPubKey     <- liftM read $ readFile "data/keys/rsa/sen_key.pub"
    theirPrivKey <- liftM read $ readFile "data/keys/rsa/rec_key.priv"

    let (header, jwe') = E.decryptJWE theirPrivKey jwt
        (Right msg')   = S.verifyJWS myPubKey jwe'

    C.putStrLn header
    C.putStrLn msg'

