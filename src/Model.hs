{-# LANGUAGE OverloadedStrings #-} 

module Model where

import Data.Aeson
import qualified Data.Aeson.Types as A
import Data.Attoparsec
import qualified Data.ByteString.Char8 as C
import Control.Applicative
import Control.Monad
import Prelude hiding (exp)
--import qualified Data.HashMap.Strict as M
import qualified Data.Text as T

data JWT = JWT {
    header    :: Header,
    claimsSet :: ClaimsSet
} deriving (Eq, Show)

data Header = Header {
    alg :: String
} deriving (Eq, Show)

data ClaimsSet = ClaimsSet {
    iss :: String,
    exp :: Integer,
    url :: Bool
} deriving (Eq, Show)

instance FromJSON Header where
    parseJSON (Object v) =
        Header <$> v .: "alg"
    parseJSON _ = mzero

instance ToJSON Header where
    toJSON (Header alg) =
        object [ "alg" .= alg ]

instance FromJSON ClaimsSet where
    parseJSON (Object v) =
        ClaimsSet <$> v .: "iss"
                  <*> v .: "exp"
                  <*> v .: "url"
    parseJSON _ = mzero
    
instance ToJSON ClaimsSet where
    toJSON (ClaimsSet iss exp url) =
        object [ "iss" .= iss
               , "exp" .= exp
               , "url" .= url ]

instance ToJSON JWT where
    toJSON (JWT header claimsSet) =
        object [ "header"    .= toJSON header
               , "claimsSet" .= toJSON claimsSet ] 

-- Header Parser
createHeader :: String -> Maybe (String, Header)
createHeader s =
    let bs = C.pack s
    in case parse json bs of 
        (Done rest r) ->
            case A.parseMaybe parseJSON r :: Maybe Header of
                (Just header) -> Just (C.unpack rest, header)
                _             -> Nothing
        _             -> Nothing

-- ClaimsSet Parser
createClaimsSet :: String -> Maybe (String, ClaimsSet)
createClaimsSet s =
    let bs = C.pack s
    in case parse json bs of 
        (Done rest r) ->
            case A.parseMaybe parseJSON r :: Maybe ClaimsSet of
                (Just claimsSet) -> Just (C.unpack rest, claimsSet)
                _                -> Nothing
        _             -> Nothing

createJWT :: String -> Maybe JWT
createJWT s = do
    (rest, header) <- createHeader s
    (_, claimsSet) <- createClaimsSet rest
    return $ JWT header claimsSet

