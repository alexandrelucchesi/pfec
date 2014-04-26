{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import           Control.Lens
import           Snap

import           Control.Concurrent.MVar (MVar)
import           Data.ByteString (ByteString)
import           Data.Time (UTCTime)
import           Model.Contract (Contract)
import           Model.UUID (UUID)
import qualified Network.HTTP.Conduit      as HC
------------------------------------------------------------------------------
data Token = Token
    { value          :: ByteString
    , service        :: UUID
    , allowedMethods :: [ByteString]
    , expiresAt      :: UTCTime
    } deriving (Eq, Show)

data App = App
    { _contract       :: Contract
    , _activeTokens   :: MVar [Token] -- TODO: Change to HashMap (Key: ServiceUUID)
    , _authURL        :: String
    , _facadeURL      :: String
    , _httpMngr       :: HC.Manager
    }

makeLenses ''App

------------------------------------------------------------------------------
type AppHandler = Handler App App

