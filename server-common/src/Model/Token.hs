{-# LANGUAGE OverloadedStrings #-}

module Model.Token where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.ByteString           (ByteString)
import           Data.Time                 (UTCTime)
import           Network.HTTP.Types.Method (Method)

import qualified CouchDB.DBService as DBService
import           Model.UUID (UUID)
import           Model.Contract (Contract)
import           Model.Service (Service)

-- | Data type representing the authorization token to be sent to the
-- client.
data Token = Token {
           uuid           :: UUID,
           revision       :: ByteString,
           value          :: ByteString,
           contractUUID   :: UUID,
           serviceUUID    :: UUID,
           allowedMethods :: [Method],
           wasUsed        :: Bool,
           expirationDate :: UTCTime
           } deriving (Eq, Show)

contract :: Token -> IO Contract
contract = undefined

service  :: Token -> IO Service
service token = DBService.findByUUID $ serviceUUID token

instance FromJSON Token where
    parseJSON (Object v) =
        Token <$> v .: "_id"
              <*> v .: "_rev"
              <*> v .: "value"
              <*> v .: "contractUUID"
              <*> v .: "serviceUUID"
              <*> v .: "allowedMethods"
              <*> v .: "wasUsed"
              <*> v .: "expirationDate"
    parseJSON _ = mzero

instance ToJSON Token where
    toJSON (Token _ _ v cu su am wu ed) =
        object [ "type"           .= ("token" :: ByteString)
               , "value"          .= v
               , "contractUUID"   .= cu
               , "serviceUUID"    .= su
               , "allowedMethods" .= am
               , "wasUsed"        .= wu
               , "expirationDate" .= ed
               ]


