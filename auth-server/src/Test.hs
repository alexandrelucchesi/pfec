{-# LANGUAGE OverloadedStrings #-}

module Test where

import           Data.Int
import           Data.ByteString (ByteString)
import qualified Data.Map          as M
import qualified Messages.RqAuth as RQA
import           Site
import           Snap.Snaplet.Test
import qualified Snap.Test         as ST

type ChallengeCredentialCode = Int64
type Credential = ByteString
testRqAuth01 :: ChallengeCredentialCode -> Credential -> IO ()
testRqAuth01 challengeCode credential = do
    let rqBuilder = ST.get "" M.empty
    resp <- runHandler message rqBuilder (handlerRqAuth request) app
    print resp
  where
    message = Just "Requests to Auth Server providing the challenge code and a credential."
    request = RQA.RqAuth01 {
                  RQA.challengeCredentialCode = challengeCode,
                  RQA.credential              = credential
              }


--type AuthorizationToken = ByteString
--type Method = ByteString
--type Service = ByteString
--testRqFacade02 :: ContractCode -> Credential -> AuthorizationToken -> Method -> Service -> IO ()
--testRqFacade02 contractCode credential authorizationToken method service = do
--    let rqBuilder =
--            case method of
--                "POST"   -> ST.postUrlEncoded service M.empty 
--                "PUT"    -> ST.put service "application/json" "{}"
--                "DELETE" -> ST.delete service M.empty
--                _        -> ST.get service M.empty
--    resp <- runHandler message rqBuilder (handlerRqFacade request) app
--    print resp
--  where
--    message = Just "Requests to Facade server providing "
--    request = RQF.RqFacade02 {
--                  RQF.contractCode       = contractCode,
--                  RQF.credential'        = credential,
--                  RQF.authorizationToken = authorizationToken
--              }



