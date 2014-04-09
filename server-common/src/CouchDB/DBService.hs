{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CouchDB.DBService where

import           Database.CouchDB.Conduit
import           Database.CouchDB.Conduit.Explicit

import           CouchDB.DBCommon
import qualified Model.Service as Service
import qualified Model.UUID as UUID

findByUUID :: UUID.UUID -> IO Service.Service
findByUUID uuid = runCouch conn $ do
    (_, service) <- couchGet dbName (UUID.toByteString' uuid) []
    return service

