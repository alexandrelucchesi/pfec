{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.ByteString   (ByteString)
import qualified Data.Map          as M
import qualified Messages.RqAuth   as RQA
import qualified Messages.RespAuth as REA
import           Site
import           Snap.Internal.Http.Types
import           Snap.Snaplet.Test
import qualified Snap.Test         as ST

import qualified Model.UUID        as UUID
import           Util.JSONWebToken

runTest :: ToJSON a => Maybe String -> a -> IO ()
runTest message request = do
    privKey <- liftM read $ readFile "../jwt-min/data/keys/rsa/sen_key.priv"
    pubKey <- serverPubKey
    let rqBuilder = do
            ST.get "" M.empty
            req <- liftIO $ signAndEncrypt privKey pubKey request 
            ST.addHeader "JWT" req 
            ST.setSecure True   -- Enables HTTPs
    resp <- runHandler message rqBuilder auth app
    either print
           (\r -> do
                putStrLn $ replicate 80 '-'
                ST.dumpResponse r                
                putStrLn $ replicate 80 '-'
                --print $ B64.decode' <$> getHeader "JWT" r
                let (r' :: Maybe REA.RespAuth) = liftM fst $
                        decrypt privKey =<< getHeader "JWT" r
                print r'
           ) resp 

-- Left the code below for the sake of anger.
-- I spent hours struggling to understand this shit of Iterators,
-- Iteratees, Enumerators, Enumeratees.
-- I made it work (code below), but then I found the 'dumpResponse'
-- function above... Fuck you!
--    case resp of
--        (Left msg) -> print msg
--        (Right resp') -> do
--            putStr "JWT Header: " >> print (getHeader "JWT" resp')
--            putStrLn "Response Body: "
--            let enumerator = rspBodyToEnum $ rspBody resp'
--                enumeratee = enumBuilderToByteString
--                iteratee = IT.printChunks True
--                newEnumerator = enumerator $= enumeratee
--            IT.runIteratee iteratee >>= IT.run_ . newEnumerator

testRqAuth01 :: ContractUUID -> IO ()
testRqAuth01 contractUUID = runTest message request
  where
    message = Just "Requests an authentication to Auth Server, providing the contract UUID"
    request = RQA.RqAuth01 {
                  RQA.contractUUID = contractUUID
              }

type ChallengeUUID = UUID.UUID
type ContractUUID = UUID.UUID
type ServiceUUID = UUID.UUID
type Credential = ByteString
testRqAuth02 :: ChallengeUUID -> ContractUUID -> ServiceUUID -> Credential -> IO ()
testRqAuth02 challengeUUID contractUUID serviceUUID credential =
    runTest message request
  where
    message = Just "Requests an authorization token to Auth Server answering the challenge: providing the challenge UUID, the contract UUID, service UUID and a credential."
    request = RQA.RqAuth02 {
                  RQA.challengeUUID = challengeUUID,
                  RQA.contractUUID' = contractUUID,
                  RQA.serviceUUID   = serviceUUID,
                  RQA.credential    = credential
              }

