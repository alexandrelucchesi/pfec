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
    {
        _facadeServerURL :: URI,
        -- Accessing the contract of the current request is a recorrent
        -- task. So we put it into the State Monad so that we can access it
        -- without querying the database again.
        -- Eg: When generating the response we need the contract's public key.
        _maybeContract   :: Maybe Contract
    }

makeLenses ''App

------------------------------------------------------------------------------

type AppHandler = Handler App App



