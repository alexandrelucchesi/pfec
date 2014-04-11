module Types where

import           Data.Text

type JWSPayload   = Text
type JWEPlaintext = Text

data JWT =
    JWS {
        jWSHeader  :: JWSHeader,
        jWSPayload :: JWSPayload
    } | JWE {
        jWEHeader    :: JWEHeader,
        jWEPlaintext :: JWEPlaintext
    } | NestedJWT {
        jWTHeader :: JWTHeader,
        jWTBody   :: JWT
    } deriving (Eq, Show)

data JWTHeader =
    JWTJWSHeader {
        jWTJWSHeader :: JWSHeader,
        jWTJWSCty    :: Cty
    } | JWTJWEHeader {
        jWTJWEHeader :: JWEHeader,
        jWTJWECty    :: Cty
    } deriving (Eq, Show)

data JWSHeader = JWSHeader {
    jWSHeaderAlg :: Alg
} deriving (Eq, Show)

data JWEHeader = JWEHeader {
    jWEHeaderAlg :: Alg,
    jWEHeaderEnc :: Enc
} deriving (Eq, Show)

data Alg = ES256
         | RSA1_5
         | RS256
    deriving (Eq, Show)

data Enc = A128CBC_HS256
    deriving (Eq, Show)

data Cty = CtyJWT
    deriving (Eq, Show)

