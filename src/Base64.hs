{-# LANGUAGE OverloadedStrings #-} 

module Base64 where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.ByteString.Base64.URL.Lazy as BL64

-- Handle trailing "=" characters (padding).
--encode :: B.ByteString -> B.ByteString
--encode = C.filter (/= '=') . B64.encode
--
--decode :: B.ByteString -> B.ByteString
--decode bs
--    | B.length bs `mod` 32 == 0 = either (C.pack . id) id $ B64.decode bs
--    | otherwise                 = decode $ C.append bs "="

encode = BL64.encode

encode' = encode . BL.fromStrict

decode bs = either error id $ BL64.decode bs

decode' = decode . BL.fromStrict

encodeIO :: IO ()
encodeIO =
    CL.readFile "temp_messages.json" >>=
    return . BL64.encode >>= print

