{-# LANGUAGE OverloadedStrings #-}

module Messages.RespAuth where

import           Data.Aeson
import           Data.ByteString (ByteString)
import           Data.Int        (Int64)
import           Data.Time       (UTCTime)

import           Model.URI
import           Model.UUID

------------------------------------------------------------------------------ | Data type holding the message's formats Auth Server can send to the client.
--
-- Messages' meanings:
--
--      RespAuth01 means:
--          "OK! I got it... You just have to reply to the
--          following address passing the following data... But hey, hurry up!
--          You don't have all time of the world or... YOU SHALL NOT PASS!
--          MWHUAhAhAhAhAhAhAhAhAhAhAhAhAhAhAhAhAhAhAhA!"
--
--      RespAuth02 means:
--          "Hmm... Indeed... You were not trying to fool me.
--          It seems you really are who you claimed to be. Before letting you do what you want,
--          let me just check if you are allowed to do that... But hey, don't
--          worry, everything's right, right?"
--
data RespAuth =
    RespAuth01 {
        replyTo        :: URI,
        credentialCode :: Int64,
        challengeUUID  :: UUID,
        expiresAt      :: UTCTime
    } | RespAuth02 {
        replyTo            :: URI,
        authorizationToken :: ByteString,
        expiresAt          :: UTCTime
    }
    deriving (Eq, Show)

instance ToJSON RespAuth where
    toJSON (RespAuth01 rt cc cu ea) =
        object [ "replyTo"        .= rt
               , "credentialCode" .= cc
               , "challengeUUID"  .= cu
               , "expiresAt"      .= ea
               ]
    toJSON (RespAuth02 rt at ea) =
        object [ "replyTo"            .= rt
               , "authorizationToken" .= at
               , "expiresAt"          .= ea
               ]

