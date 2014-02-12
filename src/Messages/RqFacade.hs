{-# LANGUAGE OverloadedStrings #-} 

module Messages.RqFacade where

import           Control.Applicative
import           Data.Aeson
import qualified Data.Text as T
import           Control.Monad

------------------------------------------------------------------------------ | Data type holding the message's formats a client can send to Facade Server.
data RqFacade =
    RqFacade01 {
        contractCode   :: Int,
        authCredential :: T.Text
    }
    deriving (Eq, Show)

instance FromJSON RqFacade where
    parseJSON (Object v) =
        RqFacade01 <$> v .: "cod_contrato"
                   <*> v .: "cred_autorizacao"
    parseJSON _ = mzero

instance ToJSON RqFacade where
    toJSON (RqFacade01 c a) =
        object [ "cod_contrato"     .= c
               , "cred_autorizacao" .= a ]


