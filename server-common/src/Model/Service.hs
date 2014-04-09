{-# LANGUAGE OverloadedStrings #-}

module Model.Service where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Char8     as C
import           Data.Map                (Map)
import           Data.Maybe                (fromJust)
import           Data.Text                 (Text)
import           Data.Word                 (Word16)
import           Database.CouchDB.Conduit (Revision)
import           Network.HTTP.Types.Method (Method)

import           Model.URI
import           Model.UUID

type Params = Map ByteString [ByteString]

data Service = Service {
             uuid        :: UUID,
             revision    :: Revision,
             description :: Text,
             host        :: ByteString,
             port        :: Word16,
             path        :: ByteString,
             methods     :: [Method],
             params      :: Params
             } deriving (Eq, Show)

url :: Service -> URI
url s = fromJust . parseURI . C.unpack $ C.concat
            ["http://", host s, ":", C.pack . show $ port s, if C.null $ path s
                                                                then ""
                                                                else '/' `C.cons` path s]

instance FromJSON Service where
    parseJSON (Object v) =
        Service <$> v .: "_id"
                <*> v .: "_rev"
                <*> v .: "description"
                <*> v .: "host"
                <*> v .: "port"
                <*> v .: "path"
                <*> v .: "methods"
                <*> v .: "params"
    parseJSON _ = mzero


instance ToJSON Service where
    toJSON (Service _ _ d h p pa m par) =
        object [ "type"        .= ("service" :: ByteString)
               , "description" .= d
               , "host"        .= h
               , "port"        .= p
               , "path"        .= pa
               , "methods"     .= m
               , "params"      .= par
               ]

