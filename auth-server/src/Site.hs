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
import qualified Data.ByteString.Char8  as C
import qualified Data.Configurator      as Config
import           Data.Maybe
import           Data.Time
import           Data.Word              (Word16)
import           Snap
import qualified System.Entropy         as Entropy
import           System.Random
------------------------------------------------------------------------------
import           Application
import qualified CouchDB.DBChallenge    as DBChallenge
import qualified CouchDB.DBContract     as DBContract
import qualified CouchDB.DBToken        as DBToken
import qualified Messages.RespAuth      as REA
import qualified Messages.RqAuth        as RQA
import qualified Model.Challenge        as Challenge
import qualified Model.Contract         as Contract
import qualified Model.Credential       as Credential
import qualified Model.Token            as Token
import           Model.URI
import qualified Util.Base64            as B64
import           Util.HttpResponse
import           Util.JSONWebToken


------------------------------------------------------------------------------ | Helper function to log things into stdout.
logStdOut :: MonadIO m => C.ByteString -> m ()
logStdOut = liftIO . C.putStrLn

------------------------------------------------------------------------------ | Sends response when everything went fine.
sendResponse :: REA.RespAuth -> AppHandler ()
sendResponse resp = do
        logStdOut "Sending response..."
        jwt <- liftIO $ toB64JSON resp
        let response = setHeader "JWT" jwt emptyResponse
        finishWith response

------------------------------------------------------------------------------ | Handler that authenticates users.
auth :: Handler App App ()
auth = do
    rq <- getRequest
    case getHeader "JWT" rq of
        Just jwtCompact -> do
            (rqAuth :: Maybe RQA.RqAuth) <- liftIO $ fromB64JSON jwtCompact
            maybe badRequest (sendResponse <=< handlerRqAuth) rqAuth
        _ -> badRequest

handlerRqAuth :: RQA.RqAuth -> AppHandler REA.RespAuth
handlerRqAuth (RQA.RqAuth01 contractUUID) =
    generateChallenge >>= makeResponse
  where
    generateChallenge :: MonadIO m => m (Challenge.Challenge, Credential.Credential)
    generateChallenge = do
        logStdOut "Generating challenge..."
        contract <- liftIO $ DBContract.findByUUID contractUUID -- TODO: Improve findByUUID to return Maybe Contract.
        let credentials = Contract.credentials contract
        index <- liftIO $ liftM (fst . randomR (0, length credentials - 1)) newStdGen
        let credential = credentials !! index
        now <- liftIO getCurrentTime

        let answer = Credential.value credential
            expiresAt = addUTCTime 3600 now
            challenge = Challenge.new contractUUID
                                      answer
                                      expiresAt

        -- Persist challenge in the database!
        challenge' <- liftIO $ DBChallenge.create challenge
        logStdOut ">> Challenge is: "
        liftIO $ print challenge'
        return (challenge', credential)

    makeResponse :: MonadSnap m => (Challenge.Challenge, Credential.Credential) -> m REA.RespAuth
    makeResponse (challenge, credential) = do
        req <- getRequest
        -- Gets its current URL.
        let url = "https://" ++ C.unpack (rqServerName req)
                             ++ ':' : show (rqServerPort req)
                             ++ "/auth"
            resp = REA.RespAuth01 {
                        REA.replyTo        = fromJust $ parseURI url,
                        REA.credentialCode = Credential.code credential,
                        REA.challengeUUID  = fromJust $ Challenge.uuid challenge,
                        REA.expiresAt      = Challenge.expiresAt challenge
                   }
        return resp

handlerRqAuth (RQA.RqAuth02 challengeUUID contractUUID serviceUUID credential) =
    verifyChallenge >>= verifyPermissions >> emitAuthToken >>= makeResponse
  where
    verifyChallenge :: AppHandler Contract.Contract
    verifyChallenge = do
        logStdOut "Verifying challenge..."
        maybeChallenge <- liftIO $ DBChallenge.findChallenge challengeUUID contractUUID

        challenge <- maybe forbidden return maybeChallenge
        logStdOut "Challenge exists..."

        now <- liftIO getCurrentTime
        unless (now < Challenge.expiresAt challenge) $
               -- TODO: Should we generate a new challenge here and send it back
               -- to the client?
               logStdOut "Specified challenge has expired!" >> forbidden

        unless (not $ Challenge.wasAnswered challenge) $
               logStdOut "It was already answered!" >> forbidden
        logStdOut "It was not answered yet..."

        challenge' <- liftIO $ DBChallenge.setAnswered challenge
        logStdOut "Changed challenge status to ANSWERED!"

        unless (credential == Challenge.answer challenge') $
               logStdOut "Answer is WRONG!" >> forbidden
        logStdOut "Answer is CORRECT!"

        liftIO $ Challenge.contract challenge'

    verifyPermissions :: (MonadSnap m) => Contract.Contract -> m ()
    verifyPermissions contract = do
        logStdOut "Verifying service permissions..."
        unless (serviceUUID `elem` Contract.allowedServicesUUIDs contract) $
               logStdOut "Access DENIED!" >> forbidden
        logStdOut "Access GRANTED!"

    emitAuthToken :: (MonadIO m) => m Token.Token
    emitAuthToken = do
        logStdOut "Emitting authorization token..."

        logStdOut "Searching for a still valid token for the service..."
        maybeToken <- liftIO $ DBToken.findAvailableToken contractUUID serviceUUID

        -- TODO: Improve design in "case of". Duplicated code.
        case maybeToken of
            Just token -> do
                now <- liftIO getCurrentTime
                if Token.expiresAt token > now
                    then do
                        logStdOut "FOUND: "
                        logStdOut $ Token.value token
                        return token
                    else do
                        logStdOut "NOT FOUND!"
                        newToken <- generateAuthToken
                        liftIO $ DBToken.create newToken -- Persist new token!
            _ -> do
                logStdOut "Contract hasn't accessed the specified service yet..."
                newToken <- generateAuthToken
                liftIO $ DBToken.create newToken -- Persist new token!
      where
        -- The method I'm using to generate random bytes can be an overhead
        -- in systems which don't have the instruction RDRAND, but it seems
        -- to be the strongest.
        -- See this answer: http://stackoverflow.com/questions/20889729/how-to-properly-generate-a-random-bytestring-in-haskell
        generateAuthToken :: (MonadIO m) => m Token.Token
        generateAuthToken = do
            logStdOut "Generating auth token..."
            tokenValue <- liftIO $ liftM B64.encode' $ Entropy.getEntropy 64 -- Size in bytes.
            now <- liftIO getCurrentTime
            let expiresAt = addUTCTime 3600 now
                allowedMethods = ["GET", "POST", "PUT", "DELETE"] -- TODO: Are we going to restrict on the HTTP method level? If so, change implementation.
            let newToken = Token.new tokenValue
                                     contractUUID
                                     serviceUUID
                                     allowedMethods
                                     expiresAt

            logStdOut "New token: "
            logStdOut tokenValue
            return newToken

    makeResponse :: Token.Token -> AppHandler REA.RespAuth
    makeResponse token = do
        url <- gets _facadeServerURL
        let resp = REA.RespAuth02 url
                                  (Token.value token)
                                  (Token.expiresAt token)
        return resp

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
    config <- liftIO $ Config.load [Config.Required "resources/devel.cfg"]
    url <- getAuthServerURL config
    addRoutes routes
    wrapSite (logStdOut (C.replicate 25 '-') *>)
    return $ App url
  where
    getAuthServerURL config = do
        host <- liftIO $ Config.lookupDefault "localhost" config "host"
        port :: Word16 <- liftIO $ Config.lookupDefault 9000 config "port"
        let maybeUrl = parseURI $ "https://" ++ host ++ ":" ++ show port
        maybe (error "Could not parse Facade Server's URL.") return maybeUrl

