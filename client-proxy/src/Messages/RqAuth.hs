{-# LANGUAGE OverloadedStrings #-} 

module Messages.RqAuth where

import           Control.Applicative
import           Data.Aeson
import qualified Data.Text as T
import           Control.Monad

------------------------------------------------------------------------------ | Data type holding the message's formats a client can send to Auth Server.
data RqAuth =
    RqAuth01 {
        credential              :: T.Text,
        challengeCredentialCode :: Int,
        contractCode            :: Int
    } | RqAuth02 {
        challengeAuthCode :: Int,
        login             :: T.Text,
        password          :: T.Text
    }
    deriving (Eq, Show)

instance FromJSON RqAuth where
    parseJSON (Object v) =
            RqAuth01 <$> v .: "credencial"
                     <*> v .: "cod_desafio"
                     <*> v .: "cod_contrato"
        <|> RqAuth02 <$> v .: "cod_desafio_auth"
                     <*> v .: "login"
                     <*> v .: "senha"
    parseJSON _ = mzero

instance ToJSON RqAuth where
    toJSON (RqAuth01 c d co) =
        object [ "credencial"   .= c
               , "cod_desafio"  .= d
               , "cod_contrato" .= co ]
    toJSON (RqAuth02 c l p) =
        object [ "cod_desafio_auth" .= c
               , "login" .= l
               , "senha" .= p ]

