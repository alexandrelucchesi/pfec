{-# LANGUAGE Trustworthy    #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CouchDB.DBChallenge where

import qualified Data.ByteString.Char8             as C
import           Data.Conduit
import           Data.Conduit.List                 as CL
import           Data.Maybe                        (fromJust, listToMaybe)
import           Database.CouchDB.Conduit
import           Database.CouchDB.Conduit.Explicit
import           Database.CouchDB.Conduit.View

import           CouchDB.DBCommon

import qualified Model.Challenge                   as Challenge
import qualified Model.UUID                        as UUID

create :: Challenge.Challenge -> IO Challenge.Challenge
create challenge = do
    uuid <- fmap UUID.toByteString' UUID.nextRandom
    runCouch conn $ do
        let rev = ""
        revision <- couchPut dbName uuid rev [] challenge
        return challenge { Challenge.uuid     = UUID.fromByteStringSafe' uuid
                         , Challenge.revision = Just revision }

type ChallengeUUID = UUID.UUID
type ContractUUID = UUID.UUID
type TokenValue = C.ByteString
findChallenge :: ChallengeUUID -> ContractUUID -> IO (Maybe Challenge.Challenge)
findChallenge challengeUUID contractUUID = do
    tokens <- runCouch conn $
        couchView_ dbName "challenge" "listChallengesWithContractUUID"
            [("key", Just $ encodeKeys $ fmap UUID.toByteString' [challengeUUID, contractUUID])] $
            rowValue =$= toType =$ CL.consume
    return $ listToMaybe tokens

setAnswered :: Challenge.Challenge -> IO Challenge.Challenge
setAnswered challenge = runCouch conn $ do
    let uuid = UUID.toByteString' $ fromJust $ Challenge.uuid challenge
        rev  = fromJust $ Challenge.revision challenge
        challenge' = challenge { Challenge.wasAnswered = True }
    revision <- couchPut dbName uuid rev [] challenge'
    return challenge' { Challenge.revision = Just revision }

