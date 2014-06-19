{-# LANGUAGE Trustworthy    #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CouchDB.DBToken where

import qualified Data.ByteString.Char8             as C
import           Data.Conduit
import           Data.Conduit.List                 as CL
import           Data.Maybe                        (listToMaybe)
import           Database.CouchDB.Conduit
import           Database.CouchDB.Conduit.Explicit
import           Database.CouchDB.Conduit.View

import           CouchDB.DBCommon

import qualified Model.Token                       as Token
import qualified Model.UUID                        as UUID

type ContractUUID = UUID.UUID
type TokenValue = C.ByteString
findToken :: ContractUUID -> TokenValue -> IO (Maybe Token.Token)
findToken contractUUID tokenValue = do
    tokens <- runCouch conn $
        couchView_ dbName "token" "listTokensWithContractUUID"
            [("key", Just $ encodeKeys [UUID.toByteString' contractUUID, tokenValue])] $
            rowValue =$= toType =$ CL.consume
    return $ listToMaybe tokens

type ServiceUUID = UUID.UUID
findAvailableToken :: ContractUUID -> ServiceUUID -> IO (Maybe Token.Token)
findAvailableToken contractUUID serviceUUID = do
    tokens <- runCouch conn $
        let uuids = fmap UUID.toByteString' [contractUUID, serviceUUID]
        in couchView_ dbName "token" "listAvailableTokens"
            [ ("endkey", Just $ encodeKeys uuids)
            , ("descending", Just "true")
            , ("limit", Just "1")
            ] -- Returns the most recent token emitted for that contract/service.
            $ rowValue =$= toType =$ CL.consume
    return $ listToMaybe tokens

create :: Token.Token -> IO Token.Token
create token = do
    uuid <- fmap UUID.toByteString' UUID.nextRandom
    runCouch conn $ do
        let rev = ""
        revision <- couchPut dbName uuid rev [] token
        return token { Token.uuid     = UUID.fromByteStringSafe' uuid
                     , Token.revision = Just revision }

