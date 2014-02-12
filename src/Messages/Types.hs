{-# LANGUAGE DeriveGeneric #-} 
{-# LANGUAGE OverloadedStrings #-} 

module Messages.Types where

import           Control.Applicative
import           Data.Aeson
import qualified Data.Text as T
import           GHC.Generics
import qualified Network.URI

------------------------------------------------------------------------------ | Data type representing a URI.
newtype URI = URI Network.URI.URI
    deriving (Eq, Generic, Show)

instance FromJSON URI where
    parseJSON = withText "URI" $
        maybe (fail "not a URI") (pure . URI) . Network.URI.parseURI . T.unpack

instance ToJSON URI where
    toJSON (URI uri) = String $ T.pack $ show uri


