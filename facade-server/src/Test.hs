{-# LANGUAGE OverloadedStrings #-}

module Test where

import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as C (map)
import           Data.Char             (toUpper)
import qualified Data.Map              as M
import qualified Data.Text             as T
import           Data.Time             (getCurrentTime)
import qualified Messages.RqFacade     as RQF
import           Site
import           Snap.Snaplet.Test
import qualified Snap.Test             as ST

import qualified CouchDB.DBContract    as DBContract
import qualified Model.Contract        as Contract
import qualified Model.UUID            as UUID

type ContractUUID = UUID.UUID
type AuthorizationToken = ByteString
type Method = ByteString
type Service = ByteString
testRqFacade01 :: ContractUUID -> AuthorizationToken -> Method -> Service -> IO ()
testRqFacade01 contractUUID authorizationToken method service = do
    let rqBuilder =
            case C.map toUpper method of
                "POST"   -> ST.postUrlEncoded service M.empty
                "PUT"    -> ST.put service "application/json" "{}"
                "DELETE" -> ST.delete service M.empty
                _        -> ST.get service M.empty
    resp <- runHandler message rqBuilder (handlerRqFacade request) app
    print resp
  where
    message = Just "Requests to Facade Server providing the contract code and an authorization token."
    request = RQF.RqFacade01 {
                    RQF.contractUUID       = contractUUID,
                    RQF.authorizationToken = authorizationToken
              }

