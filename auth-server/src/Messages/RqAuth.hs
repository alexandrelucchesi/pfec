{-# LANGUAGE OverloadedStrings #-} 

module Messages.RqAuth where

import           Control.Applicative
import           Data.Aeson
import           Data.ByteString (ByteString)
import           Control.Monad

import           Model.UUID

------------------------------------------------------------------------------ | Data type holding the message's formats a client can send to Auth Server.
data RqAuth =
    RqAuth01 {
        challengeCredentialUUID :: UUID,
        credential              :: ByteString
    } | RqAuth02 {
        challengeAuthUUID :: UUID,
        login             :: ByteString,
        password          :: ByteString
    }
    deriving (Eq, Show)

instance FromJSON RqAuth where
    parseJSON (Object v) =
            RqAuth01 <$> v .: "challengeUUID"
                     <*> v .: "credential"
        <|> RqAuth02 <$> v .: "challengeUUID"
                     <*> v .: "login"
                     <*> v .: "password"
      where
        errorMsg = "Messages.RqAuth: Could not parse UUID."
    parseJSON _ = mzero

instance ToJSON RqAuth where
    toJSON (RqAuth01 c cr) =
        object [ "challengeUUID" .= c  
               , "credential"    .= cr ]
    toJSON (RqAuth02 c l p) =
        object [ "challengeUUID" .= c
               , "login"         .= l
               , "password"      .= p ]

