{-# LANGUAGE Safe #-} 

module Util.Typeclasses where

import           Model.UUID (UUID)

class HasContractUUID a where
    getContractUUID :: a -> UUID

