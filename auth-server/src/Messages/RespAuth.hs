{-# LANGUAGE OverloadedStrings #-} 

module Messages.RespAuth where

import Control.Applicative
import Data.Aeson
import Data.ByteString (ByteString)
import Control.Monad

import Messages.Types

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
            RespAuth01 <$> v .: "cod_desafio"
                       <*> v .: "cod_usuario"
                       <*> v .: "url_servidor_autenticacao"
        <|> RespAuth02 <$> v .: "autenticado"
                       <*> v .: "nova_credencial"
    parseJSON _ = mzero

instance ToJSON RespAuth where
    toJSON (RespAuth01 d u a) =
        object [ "cod_desafio" .= d
               , "cod_usuario" .= u 
               , "url_servidor_autenticacao" .= a ]
    toJSON (RespAuth02 a c) =
        object [ "autenticado"     .= a
               , "nova_credencial" .= c ]


