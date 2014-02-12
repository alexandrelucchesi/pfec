{-# LANGUAGE OverloadedStrings #-} 

module Messages.RespFacade where

import           Control.Applicative
import           Data.Aeson
import           Control.Monad

import           Messages.Types

------------------------------------------------------------------------------ | Data type holding the message's formats Facade Server can send to the client.
data RespFacade =
    RespFacade01 {
        authServerURL           :: URI,
        challengeCredentialCode :: Int,
        credentialCode          :: Int,
        userCode                :: Int
    }
    deriving (Eq, Show)

instance FromJSON RespFacade where
    parseJSON (Object v) =
        RespFacade01 <$> v .: "url_servidor_autenticacao"
                     <*> v .: "cod_desafio"
                     <*> v .: "cod_credencial"
                     <*> v .: "cod_usuario"
    parseJSON _ = mzero

instance ToJSON RespFacade where
    toJSON (RespFacade01 u d c us) =
        object [ "url_servidor_autenticacao" .= u
               , "cod_desafio"    .= d
               , "cod_credencial" .= c
               , "cod_usuario"    .= us ]


