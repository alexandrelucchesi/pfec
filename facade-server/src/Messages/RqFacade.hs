{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}

module Messages.RqFacade where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.ByteString     (ByteString)

import           Model.UUID          (UUID)
import           Util.Typeclasses

------------------------------------------------------------------------------ | Data type holding the message's formats a client can send to Facade Server.
--
-- Messages' meanings:
--
--      RqAuth02 means:
--          "Here's my passport, you can verify it if you want
--          but, please, don't take too long,
--          I JUST FUCKING NEED TO ENTER BATCAVE IN ORDER TO SHIT!"
--
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

instance ToJSON RqFacade where
    toJSON (RqFacade01 cu at) =
        object [ "contractUUID"       .= cu
               , "authorizationToken" .= at
               ]

instance HasContractUUID RqFacade where
    getContractUUID (RqFacade01 cu _) = cu

