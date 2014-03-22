{-# LANGUAGE OverloadedStrings #-}

module Model.Service where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Char8     as C
import           Data.Char                 (isSpace)
import           Data.Maybe                (fromJust)
import           Data.Word                 (Word16)
import           Network.HTTP.Types.Method (Method)

import           Model.URI

data Service = Service {
             id      :: ByteString, -- TODO: Change to UUID.
             host    :: ByteString,
             port    :: Word16,
             path    :: ByteString,
             methods :: [Method]
             } deriving (Eq, Show)

url :: Service -> URI
url s = fromJust . parseURI . C.unpack $ C.concat
            ["http://", host s, ":", C.pack . show $ port s, if C.null $ path s
                                                                then ""
                                                                else '/' `C.cons` path s]


--instance FromJSON Service where
--        parseJSON (Object v) =
--            Service <$> v .: "host"
--                    <*> v .: "port"
--                    <*> v .: "path"
--                    <*> v .: "methods"
--        parseJSON _ = mzero

instance FromJSON Service where
    parseJSON (Object v) =
        Service <$> v .: "id"
                <*> (v .: "host" >>= \bs -> let host' = trim bs
                                            in if C.any (== ' ') host'
                                                   then fail $ "Service: parseJSON: "
                                                            ++ "Not a valid host: "
                                                            ++ C.unpack host'
                                                   else return host')
                <*> v .: "port"
                <*> (v .: "path" >>= \bs -> let path' = trim bs
                                            in if C.any (== ' ') path'
                                                    then fail $ "Service: parseJSON: "
                                                            ++ "Not a valid path: "
                                                            ++ C.unpack path'
                                                    else return path')
                <*> v .: "methods"
      where
        trim :: ByteString -> ByteString
        trim = let f = C.reverse . C.dropWhile isSpace
               in f . f

    parseJSON _ = mzero


instance ToJSON Service where
    toJSON (Service i h p pa m) =
        object [ "id"      .= i,
                 "host"    .= h,
                 "port"    .= p,
                 "path"    .= pa,
                 "methods" .= m
               ]

