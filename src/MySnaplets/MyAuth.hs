{-# LANGUAGE OverloadedStrings #-}

module MySnaplets.MyAuth
  ( AuthDB(..)
  , myAuthInit
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Snap.Snaplet

------------------------------------------------------------------------------ |
description :: Text
description = "Snaplet providing in-memory storage for a single user (ACID not supported)."


------------------------------------------------------------------------------ | Data type holding the database.
data AuthDB = AuthDB
    { credentials :: [(ByteString, ByteString)] -- Username/Password
    , authTokens  :: [ByteString] -- Grant access to services
    } deriving (Show)


------------------------------------------------------------------------------ | Snaplet's default initializer.
-- Type variable 'b' is used so that this snaplet works with any base state.
myAuthInit :: SnapletInit b AuthDB
myAuthInit = makeSnaplet "my-auth" description Nothing $
                return AuthDB
                    { credentials = [ ("alexandre", "123")
                                    , ("root", "root")
                                    ]
                    , authTokens  = [ "jky7kPn90" ] 
                    }

