{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.Configurator as Config
import qualified Data.List as L
import qualified Data.Text.Encoding as T
import           Data.Word
import           Data.Maybe
import qualified Network as HC (withSocketsDo)
import qualified Network.Connection as HC
import qualified Network.HTTP.Conduit as HC
import qualified Network.HTTP.Types.Status as HC
import           Snap
import           Snap.Snaplet.SqliteSimple
------------------------------------------------------------------------------
import           Application
import qualified BusinessLogic as Db
import qualified Util.Base64 as B64
import qualified JWT
import qualified Messages.RqAuth as RQA
import qualified Messages.RqFacade as RQF
import qualified Messages.RespAuth as REA
import qualified Messages.RespFacade as REF
import           Messages.Types hiding (fromCompactJWT, toCompactJWT)


------------------------------------------------------------------------------
-- | Print the facade URL set.
status :: Handler App App ()
status = do
    url <- gets _facadeURL
    writeBS $ C.pack url

------------------------------------------------------------------------------
-- | Show usage information.
usage :: Snap ()
usage = writeBS "GET /client/<HTTP Method>/<Service + ServiceParams>"

class FromJSON a => Resp a
instance Resp REF.RespFacade
instance Resp REA.RespAuth

------------------------------------------------------------------------------ | Parses response.
--parseResponse :: (Resp a) => Maybe String -> HC.Response b -> a
--parseResponse msg resp =
--    let res = eitherDecode . B64.decode $ jwtHeaderContents 
--    in either (error . (++) (if isJust msg then fromJust msg ++ "\n" else "")) id res
--  where
--    jwtHeaderContents =
--        let header = L.find (\(h,_) -> h == "JWT") $ HC.responseHeaders resp
--        in case header of
--                (Just (_, contents)) -> CL.fromStrict contents
--                _        -> error $ (++) (if isJust msg then fromJust msg ++ "\n" else "") "Header JWT not found."

parseResponse :: (Resp a) => Maybe String -> HC.Response b -> IO a
parseResponse msg resp = do
    res <- liftIO $ {- (fromCompactJWT . CL.toStrict . B64.decode) -} fromCompactJWT jwtHeaderContents 
    maybe (error $ if isJust msg then fromJust msg ++ "\n" else "") return res
  where
    jwtHeaderContents =
        let header = L.find (\(h,_) -> h == "JWT") $ HC.responseHeaders resp
        in case header of
                (Just (_, contents)) -> contents
                _        -> error $ (++) (if isJust msg then fromJust msg ++ "\n" else "") "Header JWT not found."

------------------------------------------------------------------------------ | Executes a request using http-conduit.
execRequest :: (ToJSON a, Resp b) => Maybe String -> a -> Maybe URI -> Handler App App b
execRequest errorMsg jwt uri = do
    m <- getParam "httpMethod"

    url <- case uri of
        (Just u) -> return $ show u
        _        -> do
            r <- getRequest
            let i = C.concat [ rqPathInfo r
                             , let q = rqQueryString r
                               in if C.null q
                                    then ""
                                    else C.append "?" q ]
            liftM (++ (C.unpack i)) getUrlPrefix

    liftIO $ putStrLn $ "URL is: " ++ show url

    manager <- gets _httpMngr
    resp <- liftIO $ HC.withSocketsDo $ do
        initReq <- HC.parseUrl url
        jwtCompact <- toCompactJWT jwt
        let hdrs = ("JWT", jwtCompact) : (HC.requestHeaders initReq)
            req = initReq { HC.checkStatus = \_ _ _ -> Nothing
                          , HC.method = fromMaybe "GET" m
                          , HC.requestHeaders = hdrs }
        liftIO $ putStrLn "============ REQUEST ==================="
        liftIO $ print req
        HC.httpLbs req manager
    liftIO $ putStrLn "============ RESPONSE =================="
    liftIO $ print resp
    liftIO $ parseResponse errorMsg resp

toCompactJWT :: ToJSON a => a -> IO C.ByteString
toCompactJWT jwtContents = do
    serverPubKey <- liftM read $ readFile "resources/rsa-server-key.pub"
    myPrivKey    <- liftM read $ readFile "resources/rsa-key.priv"
    JWT.toCompact myPrivKey serverPubKey jwtContents

fromCompactJWT :: FromJSON a => C.ByteString -> IO (Maybe a)
fromCompactJWT jwtContents = do
    myPrivKey    <- liftM read $ readFile "resources/rsa-key.priv"
    serverPubKey <- liftM read $ readFile "resources/rsa-server-key.pub"
    JWT.fromCompact myPrivKey serverPubKey jwtContents
------------------------------------------------------------------------------ | Requests Facade a service.
-- TODO: Unify rqFacade01 e rqFacade02 here!
rqFacade01 :: Handler App App REF.RespFacade

rqFacade01 = do
    let request = RQF.RqFacade01 { RQF.contractCode = 1
                                 , RQF.authCredential = "xyz123"
                                 , RQF.authorCredential = Nothing }

    -- DEBUG
    prettyWriteJSON "REQ FACADE 01" request

    execRequest (Just "Could not parse RespFacade01.") request Nothing

------------------------------------------------------------------------------ | Requests Facade a service.
rqFacade02 :: REA.RespAuth -> Handler App App (Either (IO REF.RespFacade) CL.ByteString)
rqFacade02 rq@(REA.RespAuth02 _ _) =
    if not (REA.isAuthenticated rq)
        then error errorMsg
        else do
            -- DEBUG
            prettyWriteJSON "REQ FACADE 02" request

            m <- getParam "httpMethod"
            r <- getRequest
            let i = C.concat [ rqPathInfo r
                             , let q = rqQueryString r
                               in if C.null q
                                    then ""
                                    else C.append "?" q ]
            url <- liftM (++ (C.unpack i)) getUrlPrefix
            liftIO $ putStrLn $ "URL is: " ++ show url
            manager <- gets _httpMngr  
            resp <- liftIO $ HC.withSocketsDo $ do
                initReq <- HC.parseUrl url
                jwtCompact <- toCompactJWT request
                let hdrs = ("JWT", jwtCompact) : (HC.requestHeaders initReq)
                    req = initReq { HC.checkStatus = \_ _ _ -> Nothing
                                  , HC.method = fromMaybe "GET" m
                                  , HC.requestHeaders = hdrs }
                HC.httpLbs req manager 

            return $ if HC.statusCode (HC.responseStatus resp) == 302 -- redirect
                then Left $ parseResponse (return errorMsg) resp
                else Right $ HC.responseBody resp

  where
    request = RQF.RqFacade01 { RQF.contractCode   = 1
                             , RQF.authCredential = "xyz123"
                             , RQF.authorCredential = Just $ T.decodeUtf8 $ REA.credential rq }
    errorMsg = "rqFacade02: Could not authenticate user."

rqFacade02 _ = error "rqFacade02: It shouldn't happen! :-("

------------------------------------------------------------------------------ | Requests Authentication providing specified info.
rqAuth01 :: REF.RespFacade -> Handler App App REA.RespAuth
rqAuth01 rq@(REF.RespFacade01 _ _ _ _) = do
    r <- with db $ Db.findCredentialById $ fromIntegral (REF.credentialCode rq)
    let cred = if isJust r then fromJust r else error errorMsg
        rqAuth = RQA.RqAuth01 { RQA.credential = Db.credentialCred cred
                              , RQA.challengeCredentialCode = REF.challengeCredentialCode rq
                              , RQA.contractCode = 1 }

    -- DEBUG
    prettyWriteJSON "REQ AUTH 01" rqAuth

    execRequest (Just errorMsg) rqAuth (Just $ REF.authServerURL rq)

  where
    errorMsg = "rqAuth01: Could not find credential: " ++ show (REF.credentialCode rq)

------------------------------------------------------------------------------ | Sends to Auth server the requested user credentials (login || password).
rqAuth02 :: REA.RespAuth -> Handler App App REA.RespAuth
rqAuth02 rq@(REA.RespAuth01 _ _ _) = do
    r <- with db $ Db.findUserById (fromIntegral $ REA.userCode rq)
    case r of
        Just u -> do
            let rqAuth = RQA.RqAuth02 { RQA.challengeAuthCode = REA.challengeAuthCode rq
                                      , RQA.login = Db.userLogin u
                                      , RQA.password = Db.userPassword u }

            -- DEBUG
            prettyWriteJSON "REQ AUTH 02" rqAuth

            execRequest (Just errorMsg) rqAuth (Just $ REA.authServerURL rq)
        _ -> error errorMsgDb
  where
    errorMsgDb = "rqAuth02: Could not find user with id: " ++ show (REA.userCode rq)
    errorMsg = "rqAuth02: Could not parse RespAuth02."

rqAuth02 _ = error "rqAuth02: It shouldn't happen! :-("


------------------------------------------------------------------------------
-- | Client main handler.
client :: Handler App App ()
client = do
    --liftIO $ putStrLn "\n\n\n"
    respF01 <- rqFacade01
    prettyWriteJSON "RESP FACADE 01" respF01
    respA01 <- rqAuth01 respF01
    prettyWriteJSON "RESP AUTH 01" respA01
    respA02 <- rqAuth02 respA01
    prettyWriteJSON "RESP AUTH 02" respA02
    respF02 <- rqFacade02 respA02
    case respF02 of
        Left v  -> liftIO v >>= prettyWriteJSON "RESP FACADE 02 (*ERROR*)"
        Right v -> prettyWrite "SERVICE RESPONSE" >> writeLBS v
    writeBS "\n"
    prettyWrite ("END OF CLIENT" :: String)


------------------------------------------------------------------------------
-- | Util.
getUrlPrefix :: Handler App App String
getUrlPrefix = do
    url <- gets _facadeURL
    return $ "https://" ++ url ++ "/"
        
prettyWriteJSON :: (ToJSON a) => String -> a -> Handler App App ()
prettyWriteJSON header v = do
    writeBS $ C.pack $ (take 30 $ repeat '-') ++ " " ++ header ++ " " ++ (take (60 - length header) $ repeat '-')
    writeBS "\n"
    writeBS $ CL.toStrict (encodePretty v)
    writeBS "\n"

prettyWrite :: String -> Handler App App ()
prettyWrite v = do
    writeBS $ C.pack $ (take 30 $ repeat '-') ++ " " ++ v ++ " " ++ (take (60 - length (show v)) $ repeat '-')
    writeBS "\n"

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/status", status)
         , ("/client/:httpMethod", method GET client)
         , ("/", liftSnap usage)
         ]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "client-proxy" "REST-based client." Nothing $ do
    -- config <- getSnapletUserConfig -- For some reason, it does not work
    -- properly with cabal sandboxes.
    config <- liftIO $ Config.load [Config.Required "resources/devel.cfg"]
    url    <- getFacadeURL config
    sqlite <- nestSnaplet "db" db sqliteInit
    -- According to the documentation at: 
    -- (http://hackage.haskell.org/package/http-conduit-2.0.0.7/docs/Network-HTTP-Conduit.html#t:Request), creating a new manager is an expensive operation. So, we create it only once on application start and share it accross all the requests.
    mngr   <- liftIO $ noSSLVerifyManager
    addRoutes routes
    return $ App url sqlite mngr
  where
    getFacadeURL config = do
        host <- liftIO $ Config.lookupDefault "localhost" config "host" 
        port :: Word16 <- liftIO $ Config.lookupDefault 8000 config "port"
        return $ host ++ ":" ++ show port

    noSSLVerifyManager = let tlsSettings = HC.TLSSettingsSimple {
                                -- This is where we disable certificate verification
                                HC.settingDisableCertificateValidation = True,
                                HC.settingDisableSession = False,
                                HC.settingUseServerName  = True }
                         in HC.newManager $ HC.mkManagerSettings tlsSettings Nothing

