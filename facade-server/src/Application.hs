{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import Control.Lens
import Snap
import Snap.Snaplet.SqliteSimple

------------------------------------------------------------------------------

data App = App
    { _db  :: Snaplet Sqlite
    }

makeLenses ''App

------------------------------------------------------------------------------
instance HasSqlite (Handler b App) where
    getSqliteState = with db get

type AppHandler = Handler App App



