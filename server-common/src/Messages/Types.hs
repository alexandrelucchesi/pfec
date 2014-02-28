{-# LANGUAGE DeriveGeneric #-} 
{-# LANGUAGE OverloadedStrings #-} 

module Messages.Types where

import           Control.Applicative
import           Data.Aeson
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.Text as T
import           GHC.Generics
import qualified Network.URI
import qualified Util.Base64 as B64

------------------------------------------------------------------------------ | Data type representing a URI.
newtype URI = URI Network.URI.URI
    deriving (Generic)

instance Eq URI where
    (URI u1) == (URI u2) =
        let rev = reverse . dropWhile (== '/') . reverse
            u1' = rev $ show u1
            u2' = rev $ show u2
        in u1' == u2'

instance Show URI where
    show (URI u) = show u

instance FromJSON URI where
    parseJSON = withText "URI" $
        maybe (fail "not a URI") (pure . URI) . Network.URI.parseURI . T.unpack

instance ToJSON URI where
    toJSON (URI uri) = String $ T.pack $ show uri

parseURI :: String -> Maybe URI
parseURI uri = URI <$> Network.URI.parseURI uri

------------------------------------------------------------------------------ | Converts value to JWT encoded.
toJWT :: (ToJSON a) => a -> C.ByteString
toJWT = CL.toStrict . B64.encode . encode


