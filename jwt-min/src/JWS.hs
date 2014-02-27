{-# LANGUAGE OverloadedStrings, PackageImports #-}

module JWS where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Crypto.PubKey.ECC.ECDSA as ECC
import qualified Crypto.PubKey.ECC.Generate as ECC
import qualified Crypto.PubKey.HashDescr as K
import qualified Crypto.PubKey.RSA as K
import qualified Crypto.PubKey.RSA.PKCS15 as K
import qualified Crypto.Types.PubKey.ECC as ECC
import qualified Crypto.Util as K
import qualified "crypto-random" Crypto.Random as K

import qualified Base64 as B64
import Util
import Types

getEncodedJWSHeader :: Alg -> B.ByteString
getEncodedJWSHeader alg = B64.encode . T.encodeUtf8 . T.pack $ "{\"alg\":\"" ++ show alg ++ "\"}"

sign_ECDSA :: (K.CPRG g) => g -> ECC.PrivateKey -> T.Text -> B.ByteString
sign_ECDSA g privKey msg =
    let payload = B64.encode . T.encodeUtf8 $ msg
        sigInp  = C.intercalate "." [getEncodedJWSHeader ES256, payload] -- sigInp is already in ASCII due to nature of ByteStrings (8-bit each elem).
        ((ECC.Signature r s), _) = ECC.sign g privKey (K.hashFunction K.hashDescrSHA256) sigInp
        signature = B64.encode $ K.i2bs 256 r `B.append` K.i2bs 256 s--integerToBS r `B.append` integerToBS s
    in C.intercalate "." [sigInp, signature]

--integerToBS :: Integer -> B.ByteString
--integerToBS v = let hexStr = showHex v ""
--      in B.reverse $ hexStringToBS (pad hexStr) B.empty
--    where
--        pad str -- Pad with zeroes if the integer is not a multiple of 64.
--            | length str `mod` 32 == 0 = str
--            | otherwise                = pad ('0':str)
--        hexStringToBS "" ns       = ns
--        hexStringToBS (x:y:ys) ns =
--            let [(n,_)] = readHex (x:[y])
--            in hexStringToBS ys (B.cons n ns)

signJWS :: K.PrivateKey -> T.Text -> B.ByteString
signJWS privKey msg =
    let payload = B64.encode . T.encodeUtf8 $ msg
        sigInp  = C.intercalate "." [jwsHeader, payload] -- sigInp is already in ASCII due to nature of ByteStrings (8-bit each elem).
        signature = K.sign Nothing K.hashDescrSHA256 privKey sigInp
        sig = either (error . show) B64.encode signature
    in C.intercalate "." [sigInp, sig]

verifyJWS :: K.PublicKey -> B.ByteString -> Either String T.Text
verifyJWS pubKey msg =
    let parts = C.split '.' msg
    in if length parts == 3
          then let header  = parts !!0
                   payload = parts !!1
                   sig'    = B64.decode $ parts !!2
                   msg'    = C.intercalate "." [header, payload]
               in if K.verify K.hashDescrSHA256 pubKey msg' sig'
                     then Right (T.decodeUtf8 . B64.decode $ payload)
                     else Left "Signature could not be verified."
          else Left "JWS must have 3 parts."

verify_ECDSA :: ECC.PublicKey -> B.ByteString -> Either String T.Text
verify_ECDSA pubKey msg =
    let parts = C.split '.' msg
    in if length parts == 3
       then let header  = parts !!0
                payload = parts !!1
                sig     = B64.decode $ parts !!2
                msg'    = C.intercalate "." [header, payload]
            in if B.length sig /= 64
                  then Left "Invalid ECDSA signature (length < 64 bytes)."
                  else let (r, s) = B.splitAt 32 sig
                           r' = K.bs2i r
                           s' = K.bs2i s
                           signature = ECC.Signature r' s'
                       in if ECC.verify (K.hashFunction K.hashDescrSHA256) pubKey signature msg'
                          then Right (T.decodeUtf8 . B64.decode $ payload)
                          else Left "Signature could not be verified."
       else Left "JWS must have 3 parts."

test :: IO ()
test = do
    g <- cprg
    let ((pubKey, privKey), _) = K.generate g 256 0x10001
        msg = "Live long and prosper."
        res = signJWS privKey msg
        status = verifyJWS pubKey res
    putStrLn "Is signature valid?"
    putStrLn $ "-- " ++ show status

testECC :: IO ()
testECC = do
    g <- cprg
    let ((myPubKey, myPrivKey), g') = ECC.generate g (ECC.getCurveByName ECC.SEC_p256r1)
--    myPrivKey  <- liftM read $ readFile "data/keys/ecc/sen_key.priv"
--    myPubKey   <- liftM read $ readFile "data/keys/ecc/sen_key.pub"
    
    let msg = "Hello world... Did you know? Haskell rulez!"
        res = sign_ECDSA g' myPrivKey msg
        status = verify_ECDSA myPubKey res

    print res
    print status

