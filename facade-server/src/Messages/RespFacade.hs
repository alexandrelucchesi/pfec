{-# LANGUAGE OverloadedStrings #-}

module Messages.RespFacade where

import           Data.Aeson
import           Model.URI

------------------------------------------------------------------------------ | Data type holding the message's formats Facade Server can send to the client.
data RespFacade =
    RespFacade01 {
        replyTo :: URI -- TODO: Shouldn't we use a simple HTTP redirect? (Probably...)
    } deriving (Eq, Show)

instance ToJSON RespFacade where
    toJSON (RespFacade01 rt) =
        object [ "replyTo"  .= rt ]

