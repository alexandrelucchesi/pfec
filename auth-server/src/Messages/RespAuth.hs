{-# LANGUAGE OverloadedStrings #-} 

module Messages.RespAuth where

import Control.Applicative
import Data.Aeson
import Data.ByteString (ByteString)
import Control.Monad

import Model.URI

------------------------------------------------------------------------------ | Data type holding the message's formats Auth Server can send to the client.
data RespAuth =
    RespAuth01 {
        challengeAuthCode :: Int,
        userCode          :: Int,
        authServerURL     :: URI
    } | RespAuth02 {
        isAuthenticated :: Bool,
        credential      :: ByteString
    }
    deriving (Eq, Show)

instance FromJSON RespAuth where
    parseJSON (Object v) =
            RespAuth01 <$> v .: "challenge_code"
                       <*> v .: "user_code"
                       <*> v .: "auth_server_url"
        <|> RespAuth02 <$> v .: "authenticated_and_authorized"
                       <*> v .: "new_authorization_credential"
    parseJSON _ = mzero

instance ToJSON RespAuth where
    toJSON (RespAuth01 d u a) =
        object [ "challenge_code" .= d
               , "user_code" .= u 
               , "auth_server_url" .= a ]
    toJSON (RespAuth02 a c) =
        object [ "authenticated_and_authorized" .= a
               , "new_authorization_credential" .= c ]


