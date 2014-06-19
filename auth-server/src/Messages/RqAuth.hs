{-# LANGUAGE OverloadedStrings #-}

module Messages.RqAuth where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.ByteString     (ByteString)
import           Data.Time           (UTCTime)

import           Model.UUID          (UUID)
import           Util.Typeclasses

-- | Data type holding the message's formats a client can send to Auth Server.
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
        contractUUID :: UUID,   -- ^ Contract identifier.
        sentAt       :: UTCTime -- ^ A nounce.
    } | RqAuth02 {
        challengeUUID :: UUID,       -- ^ Identifier of the question.
        contractUUID  :: UUID,       -- ^ Contract identifier.
        serviceUUID   :: UUID,       -- ^ Service contract wants to access.
        credential    :: ByteString, -- ^ Answer to the challenge.
        sentAt        :: UTCTime     -- ^ A nounce.
    }
    deriving (Eq, Show)

instance FromJSON RqAuth where
    parseJSON (Object v) =
        -- WARNING: The order of the messages are extremely important here!
        RqAuth02 <$> v .: "challengeUUID"
                 <*> v .: "contractUUID"
                 <*> v .: "serviceUUID"
                 <*> v .: "credential"
                 <*> v .: "sentAt"
        <|> RqAuth01 <$> v .: "contractUUID"
                     <*> v .: "sentAt"
    parseJSON _ = mzero

instance ToJSON RqAuth where
    toJSON (RqAuth01 cu sa) =
        object [ "contractUUID" .= cu
               , "sentAt"       .= sa
               ]
    toJSON (RqAuth02 cu cuu su cv sa) =
        object [ "challengeUUID" .= cu
               , "contractUUID"  .= cuu
               , "serviceUUID"   .= su
               , "credential"    .= cv
               , "sentAt"        .= sa
               ]

instance HasContractUUID RqAuth where
    getContractUUID (RqAuth01 cu _) = cu
    getContractUUID (RqAuth02 _ cu _ _ _) = cu

