{-# LANGUAGE OverloadedStrings #-}

module Messages.RqFacade where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.ByteString     (ByteString)

import           Model.UUID

------------------------------------------------------------------------------ | Data type holding the message's formats a client can send to Facade Server.
data RqFacade =
    RqFacade01 {
        contractUUID :: UUID,
        credential   :: ByteString
    } | RqFacade02 {
        contractUUID       :: UUID,
        credential'        :: ByteString,
        authorizationToken :: ByteString
    } deriving (Eq, Show)

instance FromJSON RqFacade where
    parseJSON (Object v) =
        RqFacade01 <$> v .: "contractUUID"
                   <*> v .: "credential"
        <|> RqFacade02 <$> v .: "contractUUID"
                       <*> v .: "credential"
                       <*> v .: "authorizationToken"
    parseJSON _ = mzero

instance ToJSON RqFacade where
    toJSON (RqFacade01 u c) =
        object [ "contractUUID" .= u
               , "credential"   .= c ]
    toJSON (RqFacade02 u c t) =
        object [ "contractUUID"       .= u
               , "credential"         .= c
               , "authorizationToken" .= t ]


