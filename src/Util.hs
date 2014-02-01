{-# LANGUAGE OverloadedStrings, PackageImports #-}

module Util where

import Control.Monad
import qualified Crypto.PubKey.RSA as K
import qualified "crypto-random" Crypto.Random as K
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Base64 as B64

cek :: (K.CPRG c) => c -> (B.ByteString, c)
cek = K.cprgGenerate 32

iv :: (K.CPRG c) => c -> (B.ByteString, c)
iv = K.cprgGenerate 16

cprg :: IO K.SystemRNG
cprg = liftM K.cprgCreate K.createEntropyPool

jweHeader :: B.ByteString
jweHeader = B64.encode . T.encodeUtf8 $ "{\"alg\":\"RSA1_5\",\"enc\":\"A128CBC-HS256\"}"

jwsHeader :: B.ByteString
jwsHeader = B64.encode . T.encodeUtf8 $ "{\"alg\":\"RS256\"}"

jwtHeader :: B.ByteString
jwtHeader = B64.encode . T.encodeUtf8 $ "{\"alg\":\"RSA1_5\",\"enc\":\"A128CBC-HS256\",\"cty\":\"JWT\"}"

genRS256Keys :: IO ()
genRS256Keys = do
    g <- cprg
    let ((recipientPubKey, recipientPrivKey), g') = K.generate g 256 0x10001
        ((senderPubKey, senderPrivKey), _) = K.generate g' 256 0x10001
    writeFile "data/keys/rec_key.pub"  $ show recipientPubKey
    writeFile "data/keys/rec_key.priv" $ show recipientPrivKey
    writeFile "data/keys/sen_key.pub"  $ show senderPubKey
    writeFile "data/keys/sen_key.priv" $ show senderPrivKey
    
