{-# LANGUAGE OverloadedStrings #-}

module Test where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C (map)
import           Data.Char (toUpper)
import qualified Data.Map          as M
import           Data.Time (getCurrentTime)
import qualified Messages.RqFacade as RQF
import           Site
--import           Snap.Internal.Http.Types (Method)
import           Snap.Snaplet.Test
import qualified Snap.Test         as ST

import qualified Db
import qualified Model.Contract as Contract
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

--testtemp :: IO ()
--testtemp = do
--    let rqBuilder = ST.get "/hello" M.empty
--    resp <- runHandler message rqBuilder temp app
--    print resp
--  where
--    message = Just "Requests to Facade Server providing only the contract code and a credential."
--
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

-- DATABASE
testCreateContract :: IO ()
testCreateContract = do
    now <- getCurrentTime
    let contract = mockContract now
    newContract <- Db.createContract contract
    print newContract
  where
    mockContract creationDate =
        Contract.Contract { Contract.uuid                 = UUID.nil
                          , Contract.revision             = ""
                          , Contract.name                 = "TJDFT"
                          , Contract.description          = "TDFT contract description."
                          , Contract.creationDate         = creationDate
                          , Contract.users                = []
                          , Contract.services             = []
                          , Contract.credentials          = []
                          , Contract.challengesCredential = []
                          , Contract.challengesAuth       = []
                          , Contract.publicKeys           = []
                          , Contract.tokens               = []
                          }

testDeleteContract :: UUID.UUID -> IO ()
testDeleteContract uuid = do
    status <- Db.deleteContractByUUID uuid
    putStrLn $ "Deleted? " ++ show status

testFindContractByUUID :: UUID.UUID -> IO ()
testFindContractByUUID uuid = do
    contract <- Db.findContractByUUID uuid
    print contract












