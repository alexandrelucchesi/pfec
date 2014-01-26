{-# LANGUAGE OverloadedStrings #-} 

import Codec.Crypto.RSA
import Crypto.Random

import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL
import Data.Maybe
import Data.Monoid
import Data.Ord
import qualified Data.Text as T

import System.IO

-- MY MODULES
import qualified Base64 as B64
import Model

encodeJSON :: ToJSON a => a -> CL.ByteString
encodeJSON = encodePretty' config
    where config = Config 4 (keyOrder ["header"] `mappend` comparing T.length)

encodeUrlB64 :: ToJSON a => a -> C.ByteString
encodeUrlB64 = B64.encode . CL.toStrict . encode --encodeJSON

encodeJWT :: JWT -> C.ByteString
encodeJWT (JWT header claimsSet) =
    encodeUrlB64 header `C.append` "." `C.append` encodeUrlB64 claimsSet 

decodeJWT :: C.ByteString -> C.ByteString
decodeJWT bs = let bsList = C.split '.' bs
                    in C.concat $ map B64.decode bsList

printHeader :: String -> IO ()
printHeader s = do
    putStrLn $ take 50 $ repeat '='
    putStrLn $ s ++ ":"
    putStrLn $ take 50 $ repeat '='

main :: IO ()
main = do
    -- #1. Reads a file containing JSON encoded data.
    handle <- openFile "src/data.json" ReadMode
    contents <- hGetContents handle
    let jwt = createJWT contents
    printHeader "JSON"
    CL.putStrLn $ encodeJSON jwt 
    printHeader "Base64 Encoded"
    C.putStrLn $ encodeJWT (fromJust jwt)
    printHeader "Base64 Decoded"
    C.putStrLn $ decodeJWT (encodeJWT (fromJust jwt))
    printHeader "Base64 Decoded To JWT"
    let jwt' = createJWT $ C.unpack $ decodeJWT (encodeJWT (fromJust jwt))
    print jwt'
    hClose handle

    -- Gets a random generator.
    -- NOTE: Never use a generator twice!!!
--    g <- newGenIO :: IO SystemRandom 
--    let (pub, priv, g1) = generateKeyPair g 2048 -- Generate keys of 256 bits.
--        msg = CL.pack "hello, world!"
--        (ct, _) = encrypt g1 pub msg
--        dt      = decrypt priv ct
--    print $ dt
--    print $ dt == msg

