{-# LANGUAGE OverloadedStrings #-}

module Base64 where

import qualified Data.ByteString            as B
import qualified Data.ByteString.Base64.URL as B64
import qualified Data.ByteString.Char8      as C

-- Handle trailing "=" characters (padding).
encode :: B.ByteString -> B.ByteString
--encode = C.filter (/= '=') . B64.encode
encode = B64.encode

decode :: B.ByteString -> B.ByteString
--decode bs
--    | B.length bs `mod` 32 == 0 = either (C.pack . id) id $ B64.decode bs
--    | otherwise                 = decode $ C.append bs "="
decode bs = either error id $ B64.decode bs


