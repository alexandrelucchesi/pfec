{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import           Control.Lens
import           Snap
import           Snap.Snaplet.SqliteSimple

import qualified Network.HTTP.Conduit      as HC
------------------------------------------------------------------------------
data App = App
    { _facadeURL :: String
    , _db        :: Snaplet Sqlite
    , _httpMngr  :: HC.Manager
    }

makeLenses ''App

instance HasSqlite (Handler b App) where
    getSqliteState = with db get


------------------------------------------------------------------------------
type AppHandler = Handler App App


