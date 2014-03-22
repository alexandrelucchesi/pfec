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
import Model.Contract
import Model.URI

------------------------------------------------------------------------------

data App = App
    { _authServerURL :: URI
    , _contracts     :: [Contract]
    }

makeLenses ''App

------------------------------------------------------------------------------

type AppHandler = Handler App App



