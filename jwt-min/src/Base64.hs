{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedStrings #-}

module Base64 where

import qualified Data.ByteString.Base64.URL as B64
import qualified Data.ByteString.Char8      as C

encode :: C.ByteString -> C.ByteString
encode = unpad . B64.encode

decode :: C.ByteString -> C.ByteString
decode bs = either error id $ B64.decode $ pad bs

pad :: C.ByteString -> C.ByteString
pad bs
    | nPads /= 0 = C.append bs $ C.replicate (4 - nPads) '='
    | otherwise  = bs
  where
    nPads = C.length bs `mod` 4

unpad :: C.ByteString -> C.ByteString
unpad = C.reverse . C.dropWhile (== '=') . C.reverse

