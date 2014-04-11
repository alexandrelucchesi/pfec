{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CouchDB.DBCommon where

import qualified Data.ByteString.Char8    as C
import           Database.CouchDB.Conduit

dbName :: C.ByteString
dbName = "protocol"

conn :: CouchConnection
conn = def

encodeKeys :: [C.ByteString] -> C.ByteString
encodeKeys xs =
    encloseInBrackets . C.intercalate "," . Prelude.map surroundByQuotes $ xs
  where
    surroundByQuotes x =
        let quotes = "\""
        in C.concat [quotes, x, quotes]
    encloseInBrackets x = '[' `C.cons` x `C.append` "]"

