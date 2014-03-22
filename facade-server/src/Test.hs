{-# LANGUAGE OverloadedStrings #-}

module Test where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C (map)
import           Data.Char (toUpper)
import qualified Data.Map          as M
import qualified Messages.RqFacade as RQF
import           Site
--import           Snap.Internal.Http.Types (Method)
import           Snap.Snaplet.Test
import qualified Snap.Test         as ST

import qualified Model.UUID as UUID

type ContractUUID = UUID.UUID
type Credential = ByteString
testRqFacade01 :: ContractUUID -> Credential -> IO ()
testRqFacade01 contractUUID credential = do
    let rqBuilder = ST.get "/hello" M.empty
    resp <- runHandler message rqBuilder (handlerRqFacade request) app
    print resp
  where
    message = Just "Requests to Facade Server providing only the contract code and a credential."
    request = RQF.RqFacade01 {
                  RQF.contractUUID = contractUUID,
                  RQF.credential   = credential
              }

type AuthorizationToken = ByteString
type Method = ByteString
type Service = ByteString
testRqFacade02 :: ContractUUID -> Credential -> AuthorizationToken -> Method -> Service -> IO ()
testRqFacade02 contractUUID credential authorizationToken method service = do
    let rqBuilder =
            case C.map toUpper method of
                "POST"   -> ST.postUrlEncoded service M.empty 
                "PUT"    -> ST.put service "application/json" "{}"
                "DELETE" -> ST.delete service M.empty
                _        -> ST.get service M.empty
    resp <- runHandler message rqBuilder (handlerRqFacade request) app
    print resp
  where
    message = Just "Requests to Facade server providing "
    request = RQF.RqFacade02 {
                  RQF.contractUUID       = contractUUID,
                  RQF.credential'        = credential,
                  RQF.authorizationToken = authorizationToken
              }


