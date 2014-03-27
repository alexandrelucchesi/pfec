{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site where
--  ( app
--  ) where

------------------------------------------------------------------------------
import           Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as C
import qualified Data.List as L
import           Data.Maybe
import           Data.Time
import qualified Safe
import qualified System.Entropy as Entropy
import           System.Random
import           Snap
------------------------------------------------------------------------------
import           Application
import qualified Db
import qualified Messages.RqAuth as RQA
import qualified Messages.RespAuth as REA
import qualified Model.Challenge as Challenge
import qualified Model.Contract as Contract
import qualified Model.Token as Token
import qualified Model.User as User
import qualified Model.UUID as UUID
import           Model.URI
import qualified Util.Base64 as B64
import           Util.HttpResponse
import           Util.JSONWebToken


------------------------------------------------------------------------------ | Handler that authenticates users.
auth :: Handler App App ()
auth = do
    rq <- getRequest
    case getHeader "JWT" rq of
        Just jwtCompact -> do
            (rqAuth :: Maybe RQA.RqAuth) <- liftIO $ fromCompactJWT jwtCompact
            maybe badRequest handlerRqAuth rqAuth
        _ -> badRequest
    
handlerRqAuth :: RQA.RqAuth -> AppHandler ()
handlerRqAuth (RQA.RqAuth01 challengeUUID credentialValue) =
    verifyChallengeCredential >>= generateChallengeAuth >>= makeResponse
  where
    verifyChallengeCredential :: AppHandler Contract.Contract
    verifyChallengeCredential = do
        liftIO $ putStrLn "Verifying credential challenge..."
        contracts <- liftIO Db.selectAllContracts
        let maybeContract =
                L.find (\c -> any (\chal -> Challenge.uuid chal == challengeUUID
                                            && Challenge.answer chal == credentialValue)
                                  (Contract.challengesCredential c))
                       contracts
        maybe forbidden return maybeContract

    generateChallengeAuth :: (MonadIO m) => Contract.Contract -> m (Challenge.ChallengeAuth, User.User)
    generateChallengeAuth contract = do
        liftIO $ putStrLn "Generating auth challenge..."
        let users = Contract.users contract
        index <- liftIO $ liftM (fst . randomR (0, length users - 1)) newStdGen
        let user = users !! index
        uuid <- liftIO UUID.nextRandom 
        now <- liftIO getCurrentTime
        let challenge = Challenge.Challenge {
            Challenge.uuid         = uuid,
            Challenge.answer       = C.concat [User.login user, "-", User.password user],
            Challenge.creationDate = now 
        }
        liftIO $ putStrLn "Challenge is..."
        liftIO $ print challenge
        -- Persist challenge in the database!
        let newChallengeList = challenge : Contract.challengesAuth contract
        liftIO $ Db.updateContract contract { Contract.challengesAuth = newChallengeList }
        return (challenge, user)

    makeResponse :: (Challenge.Challenge, User.User) -> AppHandler ()
    makeResponse (challenge, user) = do
        req <- getRequest
        let url = "https://" ++ (C.unpack $ rqServerName req)
                             ++ ':' : (show $ rqServerPort req)
                             ++ "/auth"
            resp = REA.RespAuth01 (fromJust $ parseURI url)
                                  (Challenge.uuid challenge)
                                  (User.code user)
        jwt <- liftIO $ toB64JSON resp
        let response = setHeader "JWT" jwt emptyResponse
        finishWith response

handlerRqAuth (RQA.RqAuth02 challengeUUID login password) =
    verifyChallengeAuth >>= emitAuthToken >>= makeResponse

  where
    verifyChallengeAuth :: AppHandler Contract.Contract
    verifyChallengeAuth = do
        liftIO $ putStrLn "Verifying auth challenge..."
        contracts <- liftIO Db.selectAllContracts
        let answer = C.concat [login, "-", password]
            maybeContract =
                L.find (\c -> any (\chal -> Challenge.uuid chal == challengeUUID
                                            && Challenge.answer chal == answer)
                                  (Contract.challengesAuth c))
                       contracts
        maybe forbidden return maybeContract

    emitAuthToken :: (MonadIO m) => Contract.Contract -> m Token.Token
    emitAuthToken contract = do
        liftIO $ putStrLn "Emitting authorization token..."
        liftIO $ putStrLn "Searching for a still valid token..."
        let maybeToken =
                Safe.lastMay $ L.sortBy (\t1 t2 -> Token.expirationDate t1
                                            `compare` Token.expirationDate t2)
                                        (Contract.tokens contract)
        -- TODO: Improve design in "case of". Duplicated code.
        case maybeToken of
            Just token -> do
                now <- liftIO getCurrentTime
                if Token.expirationDate token > now
                    then do
                        liftIO $ putStr ">> Found: "
                        liftIO $ C.putStrLn $ Token.value token
                        return token
                    else do
                        liftIO $ putStrLn ">> Not found."
                        newToken <- generateAuthToken
                        let newTokenList = newToken : Contract.tokens contract
                            newContract = contract { Contract.tokens = newTokenList }
                        -- Persist updated contract!
                        liftIO $ Db.updateContract newContract
                        return newToken

            _ -> do
                liftIO $ putStrLn ">> Contract has no token."
                newToken <- generateAuthToken
                let newTokenList = newToken : Contract.tokens contract
                    newContract = contract { Contract.tokens = newTokenList }
                -- Persist updated contract!
                liftIO $ Db.updateContract newContract
                return newToken
      where
        -- The method I'm using to generate random bytes can be an overhead
        -- in systems which don't have the instruction RDRAND, but it seems
        -- to be the strongest.
        -- See this answer: http://stackoverflow.com/questions/20889729/how-to-properly-generate-a-random-bytestring-in-haskell
        generateAuthToken :: (MonadIO m) => m Token.Token
        generateAuthToken = do
            liftIO $ putStrLn "Generating auth token..."
            token <- liftIO $ liftM B64.encode' $ Entropy.getEntropy 64 -- Size in bytes.
            now <- liftIO getCurrentTime
            let newToken = Token.Token {
                                Token.value          = token,
                                Token.creationDate   = now,
                                Token.expirationDate = addUTCTime 3600 now
                           }
            liftIO $ putStr ">> New token: "
            liftIO $ C.putStrLn token
            return newToken

    makeResponse :: Token.Token -> AppHandler ()
    makeResponse token = do
        let resp = REA.RespAuth02 (Token.value token)
                                  (Token.expirationDate token)
        jwt <- liftIO $ toB64JSON resp
        let response = setHeader "JWT" jwt emptyResponse
        finishWith response
    
------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(C.ByteString, Handler App App ())]
routes = [ ("/auth", auth)
         , ("/", auth)
         ]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "auth-server" "REST-based authentication server." Nothing $ do
    addRoutes routes
    return App

