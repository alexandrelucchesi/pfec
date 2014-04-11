{-# LANGUAGE OverloadedStrings #-}

module Model where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Attoparsec
import           Data.Maybe
import           Prelude               hiding (exp)

import qualified Data.Aeson.Types      as A
import qualified Data.ByteString.Char8 as C
import qualified Data.HashMap.Strict   as M
import qualified Data.Text             as T
import qualified Network.URI

-- Datatypes
data JWT = JWT {
    header    :: Header,
    claimsSet :: ClaimsSet
} deriving (Eq, Show)

data Header = Header {
    alg :: String
} deriving (Eq, Show)

data JWEHeader = JWEHeader {
    algJWE :: String,
    encJWE :: String,
    ctyJWE :: String
} deriving (Eq, Show)

data ClaimsSet = ClaimsSet {
    iss                :: StringOrURI,
    exp                :: Integer, -- TODO: Change to some "IntDate".
    unregisteredClaims :: M.HashMap T.Text Value
} deriving (Eq, Show)

data StringOrURI = Arbitrary T.Text
                 | OrURI URI
                 deriving (Eq, Show)

newtype URI = URI Network.URI.URI
    deriving (Eq, Show)

-- Functions
createJWT :: String -> Maybe JWT
createJWT s = do
    (rest, h) <- createHeader s
    (_, cs) <- createClaimsSet rest
    return $ JWT h cs

-- JWEHeader parser
createJWEHeader :: String -> Maybe (String, JWEHeader)
createJWEHeader s =
    let bs = C.pack s
    in case parse json bs of
        (Done rest r) ->
            case A.parseMaybe parseJSON r :: Maybe JWEHeader of
                (Just h) -> Just (C.unpack rest, h)
                _             -> Nothing
        _             -> Nothing

-- Header parser
createHeader :: String -> Maybe (String, Header)
createHeader s =
    let bs = C.pack s
    in case parse json bs of
        (Done rest r) ->
            case A.parseMaybe parseJSON r :: Maybe Header of
                (Just h) -> Just (C.unpack rest, h)
                _             -> Nothing
        _             -> Nothing

-- ClaimsSet parser
createClaimsSet :: String -> Maybe (String, ClaimsSet)
createClaimsSet s =
    let bs = C.pack s
    in case parse json bs of
        (Done rest r) ->
            case A.parseMaybe parseJSON r :: Maybe ClaimsSet of
                (Just cs) -> Just (C.unpack rest, cs)
                _                -> Nothing
        _             -> Nothing

-- Instance Declarations
instance ToJSON JWT where
    toJSON (JWT h cs) =
        object [ "header"    .= toJSON h
               , "claimsSet" .= toJSON cs ]

instance FromJSON Header where
    parseJSON (Object v) =
        Header <$> v .: "alg"
    parseJSON _ = mzero

instance ToJSON Header where
    toJSON (Header a) =
        object [ "alg" .= a ]

instance FromJSON JWEHeader where
    parseJSON (Object v) =
        JWEHeader <$> v .: "alg"
                  <*> v .: "enc"
                  <*> v .: "cty"
    parseJSON _ = mzero

instance ToJSON JWEHeader where
    toJSON (JWEHeader a e c) =
        object [ "alg" .= a
               , "enc" .= e
               , "cty" .= c ]

filterUnregistered :: M.HashMap T.Text Value -> M.HashMap T.Text Value
filterUnregistered = M.filterWithKey (\k _ -> k `notElem` registered) where
    registered = ["iss", "exp"]

instance FromJSON ClaimsSet where
  parseJSON = withObject "JWT Claims Set" (\o -> ClaimsSet
    <$> o .: "iss"
    <*> o .: "exp"
    <*> pure (filterUnregistered o))

instance ToJSON ClaimsSet where
    toJSON (ClaimsSet i e o) = object $
        [ "iss" .= i
        , "exp" .= e ] ++ M.toList (filterUnregistered o)

instance FromJSON StringOrURI where
    parseJSON = withText "StringOrURI" (\s ->
        if T.any (== ':') s
            then OrURI <$> parseJSON (String s)
            else pure $ Arbitrary s)

instance ToJSON StringOrURI where
    toJSON (Arbitrary s) = toJSON s
    toJSON (OrURI uri)   = toJSON $ show uri

instance FromJSON URI where
    parseJSON = withText "URI" $
        maybe (fail "not a URI") (pure . URI) . Network.URI.parseURI . T.unpack

instance ToJSON URI where
    toJSON (URI uri) = String $ T.pack $ show uri

