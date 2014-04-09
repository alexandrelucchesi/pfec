{-# LANGUAGE OverloadedStrings #-} 

module Messages.RespAuth where

import Control.Applicative
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Time (UTCTime)
import Control.Monad

import Model.UUID
import Model.URI

------------------------------------------------------------------------------ | Data type holding the message's formats Auth Server can send to the client.
data RespAuth =
    RespAuth01 {
        authServerURL     :: URI,
        challengeCode     :: UUID,
        credentialCode    :: Int64,
        expirationDate    :: UTCTime
    } | RespAuth02 {
        authorizationToken    :: ByteString,
        serviceExpirationDate :: UTCTime
    }
    deriving (Eq, Show)

instance FromJSON RespAuth where
    parseJSON (Object v) =
            RespAuth01 <$> v .: "authServerURL"
                       <*> v .: "challengeCode"
                       <*> v .: "userCode"
        <|> RespAuth02 <$> v .: "authorizationToken"
                       <*> v .: "expirationDate"
    parseJSON _ = mzero

instance ToJSON RespAuth where
    toJSON (RespAuth01 a c u) =
        object [ "authServerURL" .= a
               , "challengeCode" .= c 
               , "userCode"      .= u
               ]
    toJSON (RespAuth02 a e) =
        object [ "authorizationToken" .= a
               , "expirationDate"     .= e
               ]


