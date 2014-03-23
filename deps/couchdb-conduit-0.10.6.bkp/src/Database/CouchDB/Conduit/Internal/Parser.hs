{-# LANGUAGE OverloadedStrings #-}

module Database.CouchDB.Conduit.Internal.Parser where

import Control.Exception.Lifted (throw)

import Data.Conduit (MonadResource)
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as M
import qualified Data.Aeson as A
import Data.String.Conversions ((<>), cs)

import Database.CouchDB.Conduit.Internal.Connection 
                        (CouchError(..), Revision)

extractField :: T.Text -> A.Value -> Either CouchError A.Value
extractField s (A.Object o) = 
    maybe (Left $ CouchInternalError $ "unable to find field " <> cs s)
          Right $ M.lookup s o
extractField _ _ = Left $ CouchInternalError "Couch DB did not return an object"

extractRev :: A.Value -> Either CouchError Revision
extractRev = look . extractField "rev"
  where 
    look (Right (A.String a)) = Right $ cs a
    look _ = Left $ CouchInternalError "CouchDB object has't revision"

-- | Convert to type with given convertor
jsonToTypeWith :: MonadResource m =>
                (A.Value -> A.Result a) 
             -> A.Value 
             -> m a
jsonToTypeWith f j = case f j of
        A.Error e -> throw $ CouchInternalError $ 
                        "Error parsing json: " <> cs e
        A.Success o -> return o

