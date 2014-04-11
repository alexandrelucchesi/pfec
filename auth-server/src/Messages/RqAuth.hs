{-# LANGUAGE OverloadedStrings #-}

module Messages.RqAuth where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.ByteString     (ByteString)

import           Model.UUID          (UUID)

------------------------------------------------------------------------------ | Data type holding the message's formats a client can send to Auth Server.
--
-- Messages' meanings:
--
--      RqAuth01 means:
--          "Hey, I want to authenticate! I'm Batman..."
--
--      RqAuth02 means:
--          "Here's the data you asked! Do you believe me now?
--          And also, I want to access the service identified by 'serviceUUID'
--          please..."
data RqAuth =
    RqAuth01 {
        contractUUID :: UUID
    } | RqAuth02 {
        challengeUUID :: UUID,
        contractUUID' :: UUID,
        serviceUUID   :: UUID,
        credential    :: ByteString
    }
    deriving (Eq, Show)

instance FromJSON RqAuth where
    parseJSON (Object v) =
        -- WARNING: The order of the messages are extremely important here!
        RqAuth02 <$> v .: "challengeUUID"
                 <*> v .: "contractUUID"
                 <*> v .: "serviceUUID"
                 <*> v .: "credential"
        <|> RqAuth01 <$> v .: "contractUUID"
    parseJSON _ = mzero

instance ToJSON RqAuth where
    toJSON (RqAuth01 cu) =
        object [ "contractUUID" .= cu
               ]
    toJSON (RqAuth02 cu cuu su cv) =
        object [ "challengeUUID" .= cu
               , "contractUUID"  .= cuu
               , "serviceUUID"   .= su
               , "credential"    .= cv
               ]
