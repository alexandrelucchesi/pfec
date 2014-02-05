{-# LANGUAGE DeriveGeneric, OverloadedStrings #-} 

module Messages where

import Control.Applicative
import Control.Monad
import Data.Aeson
import qualified Data.Aeson.Types as A
import Data.Attoparsec.ByteString
import qualified Data.ByteString as B
import qualified Data.Text as T
import GHC.Generics
import qualified Network.URI

newtype URI = URI Network.URI.URI
    deriving (Eq, Generic, Show)

instance FromJSON URI where
    parseJSON = withText "URI" $
        maybe (fail "not a URI") (pure . URI) . Network.URI.parseURI . T.unpack

instance ToJSON URI where
    toJSON (URI uri) = String $ T.pack $ show uri

data MsgFmt02 = MsgFmt02 {
    urlC          :: URI,
    codCredential :: Int
} deriving (Eq, Generic, Show)

instance FromJSON MsgFmt02
instance ToJSON MsgFmt02

data MsgFmt03 = MsgFmt03 {
    credential    :: T.Text
} deriving (Eq, Generic, Show)

instance FromJSON MsgFmt03
instance ToJSON MsgFmt03

data MsgFmt04 = MsgFmt04 {
    codContract   :: Int,
    codUser       :: Int
} deriving (Eq, Generic, Show)

instance FromJSON MsgFmt04
instance ToJSON MsgFmt04

data MsgFmt05 = MsgFmt05 {
    codChallenge  :: Int,
    user          :: T.Text,
    password      :: T.Text
} deriving (Eq, Generic, Show)

instance FromJSON MsgFmt05
instance ToJSON MsgFmt05

data MsgFmt06 = MsgFmt06 {
    newCredential :: T.Text
} deriving (Eq, Generic, Show)

instance FromJSON MsgFmt06
instance ToJSON MsgFmt06

data MsgFmt07 = MsgFmt07 {
    newCred       :: T.Text,
    urlA          :: URI,
    dateTime      :: String -- FIXME: Use a proper type.
} deriving (Eq, Generic, Show)

instance FromJSON MsgFmt07
instance ToJSON MsgFmt07
        
sampleMsgs :: Parser [Value]
sampleMsgs = replicateM 6 json

test :: IO ()
test = do
    text <- B.readFile "messages.json"
    let results = parse sampleMsgs text
    case results of
        (Done _ values) ->
            if length values == 6
                then do
                     let (Success msg02) = (fromJSON $ values !! 0) :: A.Result MsgFmt02
                         (Success msg03) = (fromJSON $ values !! 1) :: A.Result MsgFmt03
                         (Success msg04) = (fromJSON $ values !! 2) :: A.Result MsgFmt04
                         (Success msg05) = (fromJSON $ values !! 3) :: A.Result MsgFmt05
                         (Success msg06) = (fromJSON $ values !! 4) :: A.Result MsgFmt06
                         (Success msg07) = (fromJSON $ values !! 5) :: A.Result MsgFmt07
                     print msg02
                     print msg03
                     print msg04
                     print msg05
                     print msg06
                     print msg07
                else print "Else? Weird error occurred."

