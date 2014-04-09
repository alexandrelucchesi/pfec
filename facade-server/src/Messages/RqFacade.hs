{-# LANGUAGE OverloadedStrings #-}

module Messages.RqFacade where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.ByteString     (ByteString)

import           Model.UUID          (UUID)

------------------------------------------------------------------------------ | Data type holding the message's formats a client can send to Facade Server.
data RqFacade =
    RqFacade01 {
        contractUUID       :: UUID,
        authorizationToken :: ByteString
    } deriving (Eq, Show)

instance FromJSON RqFacade where
    parseJSON (Object v) =
        RqFacade01 <$> v .: "contractUUID"
                   <*> v .: "authorizationToken"
    parseJSON _ = mzero

