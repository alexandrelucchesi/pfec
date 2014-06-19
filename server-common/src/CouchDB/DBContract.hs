{-# LANGUAGE Trustworthy    #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CouchDB.DBContract where

import           Control.Monad.IO.Class            (MonadIO, liftIO)
import           Database.CouchDB.Conduit
import           Database.CouchDB.Conduit.Explicit

import           CouchDB.DBCommon
import qualified Model.Contract                    as Contract (Contract (..))
import qualified Model.UUID                        as UUID (UUID (..),
                                                            nextRandom,
                                                            toByteString')

-- TODO: Catch exceptions.
findByUUID :: UUID.UUID -> IO Contract.Contract
findByUUID uuid = runCouch conn $ do
    (_, contract) <- couchGet dbName (UUID.toByteString' uuid) []
    return contract

--newtype DocId = DocId C.ByteString
--    deriving (Eq, Show)
--
--instance FromJSON DocId where
--    parseJSON (Object v) =
--        DocId <$> v .: "id"
--
--data AllDocs = AllDocs [DocId]
--    deriving (Eq, Show)
--
--instance FromJSON AllDocs where
--    parseJSON (Object v) =
--        AllDocs <$> v .: "rows"

selectAll :: IO [Contract.Contract]
selectAll = do
    contract <- findByUUID "b3ea803d-d16f-47d8-89fa-61dac3795487"
    return [contract]

create :: Contract.Contract -> IO Contract.Contract
create contract = do
    uuid <- liftIO UUID.nextRandom
    update contract { Contract.uuid     = uuid
                    , Contract.revision = "" }

update :: Contract.Contract -> IO Contract.Contract
update contract = runCouch conn $ do
    let uuid = UUID.toByteString' $ Contract.uuid contract
        rev  = Contract.revision contract
    revision <- couchPut dbName uuid rev [] contract
    return contract { Contract.revision = revision }

deleteByUUID :: UUID.UUID -> IO Bool
deleteByUUID uuid = runCouch conn $ do
    contract <- liftIO $ findByUUID uuid
    couchDelete dbName (UUID.toByteString' uuid) (Contract.revision contract)
    return True


