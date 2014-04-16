{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import           Control.Lens
import           Model.Contract    (Contract)
import           Model.URI    (URI)
import           Snap

------------------------------------------------------------------------------

data App = App
    { _authServerURL :: URI
    , _maybeContract :: Maybe Contract
    }

makeLenses ''App

------------------------------------------------------------------------------

type AppHandler = Handler App App




