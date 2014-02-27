{-# LANGUAGE OverloadedStrings #-}

module MySnaplets.MyAuth where
--  ( AuthDB(..)
--  , myAuthInit
--  ) where
--
import Data.ByteString (ByteString)
import Data.Text (Text)
import Snap.Snaplet

------------------------------------------------------------------------------ |
description :: Text
description = "Snaplet providing in-memory storage for a single user (ACID not supported)."

------------------------------------------------------------------------------ | Model
data Credential = Credential {
    credContractCode  :: Int,
    credCodUser       :: Int,
    credCodCredential :: Int,
    credCredential :: Text
} deriving (Eq, Show)

data Challenge = Challenge {
    chalChallengeCode :: Int,
    chalContractCode  :: Int, 
    chalUserCode      :: Int,
    chalChallengeResponse :: Text,
    chalDateTime          :: String -- TODO: Use Datetime.
} deriving (Eq, Show)

--saveChallenge :: Int -> Handler b AuthDB Challenge
--saveChallenge credCode = do
--    db <- get
--    let credList = tbChallenge db
--        cred = find (\c -> credCodCrential c == credCode) (tbCredential db)
--    case cred of
--        Nothing -> error $ "It shouldn't happen! :-("
--        _       -> tbChallenge db


------------------------------------------------------------------------------ | Data type holding the database.
data AuthDB = AuthDB
    { credentials :: [(ByteString, ByteString)] -- Username/Password
    , authTokens  :: [ByteString] -- Grant access to services
    , tbCredential :: [Credential]
    , tbChallenge  :: [Challenge]
    } deriving (Show)


------------------------------------------------------------------------------ | Snaplet's default initializer.
-- Type variable 'b' is used so that this snaplet works with any base state.
myAuthInit :: SnapletInit b AuthDB
myAuthInit = makeSnaplet "my-auth" description Nothing $
                return AuthDB
                    { credentials = [ ("alexandre", "123")
                                    , ("root", "root")
                                    ]
                    , authTokens  = [ "jky7kPn90" ] 
                    }

