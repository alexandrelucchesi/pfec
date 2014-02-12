{-# LANGUAGE OverloadedStrings #-} 

module Messages.RespAuth where

import Control.Applicative
import Data.Aeson
import qualified Data.Text as T
import Control.Monad

------------------------------------------------------------------------------ | Data type holding the message's formats Auth Server can send to the client.
data RespAuth =
    RespAuth01 {
        challengeAuthCode :: Int,
        userCode          :: Int
    } | RespAuth02 {
        credential :: T.Text
    }
    deriving (Eq, Show)

instance FromJSON RespAuth where
    parseJSON (Object v) =
            RespAuth01 <$> v .: "cod_desafio"
                       <*> v .: "cod_usuario"
        <|> RespAuth02 <$> v .: "nova_credencial"
    parseJSON _ = mzero

instance ToJSON RespAuth where
    toJSON (RespAuth01 d u) =
        object [ "cod_desafio" .= d
               , "cod_usuario" .= u ]
    toJSON (RespAuth02 c) =
        object [ "nova_credencial" .= c ]
