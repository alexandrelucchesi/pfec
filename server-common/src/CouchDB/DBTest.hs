{-# LANGUAGE OverloadedStrings #-}

module Test where

import qualified Data.Text          as T
import           Data.Time          (getCurrentTime)

import qualified CouchDB.DBContract as DBContract
import qualified Model.Contract     as Contract
import qualified Model.UUID         as UUID

testCreateContract :: T.Text -> IO ()
testCreateContract name = do
    now <- getCurrentTime
    let contract = mockContract now
    newContract <- DBContract.create contract
    print newContract
  where
    mockContract creationDate =
        Contract.Contract { Contract.uuid                 = UUID.nil
                          , Contract.revision             = ""
                          , Contract.name                 = name
                          , Contract.description          = "TDFT contract description."
                          , Contract.creationDate         = creationDate
                          , Contract.credentials          = []
                          , Contract.allowedServicesUUIDs = []
                          , Contract.publicKeys           = []
                          }

testDeleteContract :: UUID.UUID -> IO ()
testDeleteContract uuid = do
    status <- DBContract.deleteByUUID uuid
    putStrLn $ "Deleted? " ++ show status

testFindContractByUUID :: UUID.UUID -> IO ()
testFindContractByUUID uuid = do
    contract <- DBContract.findByUUID uuid
    print contract

