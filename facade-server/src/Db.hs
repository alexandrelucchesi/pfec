{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Db where

import           Data.Aeson
import           Control.Applicative
import           Control.Monad.IO.Class            (MonadIO, liftIO)
import           Control.Monad
--import Control.Monad.Trans.Resource (MonadThrow, MonadUnsafeIO)
--import Control.Monad.Trans.Control (MonadBaseControl)
import           Data.ByteString                   (ByteString)
import           Database.CouchDB.Conduit
import           Database.CouchDB.Conduit.Explicit

conn :: CouchConnection
conn = def

data User = User {
    name :: String
} deriving (Eq, Show)

instance FromJSON User where
    parseJSON (Object v) =
        User <$> v .: "name"
    parseJSON _ = mzero

instance ToJSON User where
    toJSON (User n) =
        object [ "name" .= n ]

couchTest :: IO ()
-- couchTest :: IO () -- restricting it to IO is also an option
couchTest = runCouch conn $ do
    rev <- couchPut "mydb" "doc" "1-6c76a52aea92ddbbbdc5d0bc0dfb3f44" [] $ User { name = "alexandre lucchesi" }
--    (rev, user :: User) <- couchGet "mydb" "doc" []
    liftIO $ putStrLn $ "Rev: " ++ show rev
--    liftIO $ putStrLn $ "User info: " ++ show user
--    couchPut' "mydb" "my-doc1" [] $ D 12345 "third"    -- notice - no rev
--    rev3 <- couchRev "mydb" "my-doc1"
--    couchDelete "mydb" "my-doc1" rev3

