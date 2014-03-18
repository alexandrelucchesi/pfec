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
        contractCode :: UUID,
        credential   :: ByteString
    } | RqFacade02 {
        contractCode       :: UUID,
        credential'        :: ByteString,
        authorizationToken :: ByteString
    } deriving (Eq, Show)

instance FromJSON RqFacade where
    parseJSON (Object v) = -- WARNING: The order RqFacade02 -> RqFacade01 matters.
            RqFacade02 <$> v .: "contractUUID"
                       <*> v .: "credential"
                       <*> v .: "authorizationToken"
        <|> RqFacade01 <$> v .: "contractUUID"
                       <*> v .: "credential"
    parseJSON _ = mzero

instance ToJSON RqFacade where
    toJSON (RqFacade01 cc c) =
        object [ "contractUUID" .= cc
               , "credential"   .= c ]
    toJSON (RqFacade02 cc c at) =
        object [ "contractUUID"       .= cc
               , "credential"         .= c 
               , "authorizationToken" .= at ]


