{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Db where

import           Control.Applicative
import           Control.Monad.IO.Class            (MonadIO, liftIO)
import           Control.Monad
import           Data.Aeson 
import qualified Data.ByteString.Char8 as C
import           Database.CouchDB.Conduit
import           Database.CouchDB.Conduit.Explicit

import qualified Model.Contract as Contract
import qualified Model.UUID as UUID

dbName :: C.ByteString
dbName = "contracts"

conn :: CouchConnection
conn = def

-- TODO: Catch exceptions.
findContractByUUID :: UUID.UUID -> IO Contract.Contract
findContractByUUID uuid = runCouch conn $ do
    (_, contract) <- couchGet dbName (UUID.toByteString' uuid) []
    return contract

newtype DocId = DocId C.ByteString
    deriving (Eq, Show)

instance FromJSON DocId where
    parseJSON (Object v) =
        DocId <$> v .: "id"

data AllDocs = AllDocs [DocId]
    deriving (Eq, Show)

instance FromJSON AllDocs where
    parseJSON (Object v) =
        AllDocs <$> v .: "rows"

selectAllContracts :: IO [Contract.Contract]
selectAllContracts = do
    contract <- findContractByUUID "b3ea803d-d16f-47d8-89fa-61dac3795487"
    return [contract]

createContract :: Contract.Contract -> IO Contract.Contract
createContract contract = do
    uuid <- liftIO $ UUID.nextRandom 
    updateContract contract { Contract.uuid = uuid
                            , Contract.revision = "" }

updateContract :: Contract.Contract -> IO Contract.Contract
updateContract contract = runCouch conn $ do
    let uuid = UUID.toByteString' $ Contract.uuid contract
        rev  = Contract.revision contract
    revision <- couchPut dbName uuid rev [] contract
    return contract { Contract.revision = revision }

deleteContractByUUID :: UUID.UUID -> IO Bool
deleteContractByUUID uuid = runCouch conn $ do
    contract <- liftIO $ findContractByUUID uuid
    couchDelete dbName (UUID.toByteString' uuid) (Contract.revision contract)
    return True

