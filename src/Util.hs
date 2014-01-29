{-# LANGUAGE OverloadedStrings, PackageImports #-}

module Util where

import Control.Monad
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

