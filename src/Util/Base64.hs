{-# LANGUAGE OverloadedStrings #-} 

module Util.Base64 where

import           Control.Monad
import           Data.ByteString (ByteString)
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

encode :: BL.ByteString -> BL.ByteString
encode = BL64.encode

encode' :: ByteString -> BL.ByteString
encode' = encode . BL.fromStrict

decode :: BL.ByteString -> BL.ByteString
decode bs = either error id $ BL64.decode bs

decode' :: ByteString -> BL.ByteString
decode' = decode . BL.fromStrict

encodeIO :: FilePath -> IO ()
encodeIO path = liftM BL64.encode (CL.readFile path) >>= print


