{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Challenge where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.ByteString     (ByteString)
import           Data.Time           (UTCTime)

import qualified CouchDB.DBContract as DBContract
import           Model.UUID          (UUID (..))
import           Model.Contract      (Contract (..))

data Challenge = Challenge {
               uuid         :: Maybe UUID,
               revision     :: Maybe ByteString,
               wasAnswered  :: Bool,
               contractUUID :: UUID,
               answer       :: ByteString,
               expiresAt    :: UTCTime
               } deriving (Eq, Show)

new :: UUID -> ByteString -> UTCTime -> Challenge
new = Challenge Nothing Nothing False

contract :: Challenge -> IO Contract
contract = DBContract.findByUUID . contractUUID

instance FromJSON Challenge where
    parseJSON (Object v) =
        Challenge <$> v .: "_id"
                  <*> v .: "_rev"
                  <*> v .: "wasAnswered"
                  <*> v .: "contractUUID"
                  <*> v .: "answer"
                  <*> v .: "expiresAt"
    parseJSON _ = mzero

instance ToJSON Challenge where
    toJSON (Challenge _ _ wa cu a ea) =
        object [ "type"         .= ("challenge" :: ByteString)
               , "wasAnswered"  .= wa
               , "contractUUID" .= cu
               , "answer"       .= a
               , "expiresAt"    .= ea
               ]

