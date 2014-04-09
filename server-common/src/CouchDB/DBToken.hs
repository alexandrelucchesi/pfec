{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CouchDB.DBToken where

import           Data.Conduit
import           Data.Conduit.List as CL
import qualified Data.ByteString.Char8 as C
import           Data.Maybe (listToMaybe)
import           Database.CouchDB.Conduit
import           Database.CouchDB.Conduit.Explicit
import           Database.CouchDB.Conduit.View

import           CouchDB.DBCommon

import qualified Model.UUID as UUID
import qualified Model.Token as Token

type ContractUUID = UUID.UUID
type TokenValue = C.ByteString
findToken :: ContractUUID -> TokenValue -> IO (Maybe Token.Token)
findToken contractUUID tokenValue = do
    tokens <- runCouch conn $
        couchView_ dbName "token" "listTokensWithContractUUID"
            [("key", Just $ encodeKeys [UUID.toByteString' contractUUID, tokenValue])] $
            rowValue =$= toType =$ CL.consume
    return $ listToMaybe tokens

setUsed :: Token.Token -> IO ()
setUsed token = runCouch conn $ do
    let uuid = UUID.toByteString' $ Token.uuid token
        rev  = Token.revision token
    couchPut dbName uuid rev [] $ token { Token.wasUsed = True }
    return ()







