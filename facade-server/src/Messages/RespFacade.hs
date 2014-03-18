{-# LANGUAGE OverloadedStrings #-} 

module Messages.RespFacade where

import           Control.Applicative
import           Data.Aeson
import           Control.Monad

import           Model.URI
import           Model.UUID

------------------------------------------------------------------------------ | Data type holding the message's formats Facade Server can send to the client.
data RespFacade =
    RespFacade01 {
        authServerURL  :: URI,
        challengeCode  :: UUID,
        credentialCode :: Int
    } deriving (Eq, Show)

instance FromJSON RespFacade where
    parseJSON (Object v) =
        RespFacade01 <$> v .: "authServerURL"
                     <*> v .: "challengeUUID"
                     <*> v .: "credentialCode"
    parseJSON _ = mzero

instance ToJSON RespFacade where
    toJSON (RespFacade01 asu cc cco) =
        object [ "authServerURL"  .= asu
               , "challengeUUID"  .= cc 
               , "credentialCode" .= cco ]


