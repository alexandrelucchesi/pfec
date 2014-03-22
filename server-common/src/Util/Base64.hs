{-# LANGUAGE OverloadedStrings #-} 

module Util.Base64 where

import           Control.Monad
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.ByteString.Base64.URL.Lazy as BL64
import qualified Data.ByteString.Base64.URL as B64

-- Always encode to a strict bytestring (snap's response).
encode :: CL.ByteString -> C.ByteString
encode = CL.toStrict . BL64.encode

encode' :: C.ByteString -> C.ByteString
encode' = B64.encode

-- Always decode to a lazy bytestring (aeson's input).
decode :: CL.ByteString -> CL.ByteString
decode bs = either error id $ BL64.decode bs

decode' :: C.ByteString -> CL.ByteString
decode' bs = either error CL.fromStrict $ B64.decode bs

encodeIO :: FilePath -> IO ()
encodeIO path = liftM BL64.encode (CL.readFile path) >>= print

