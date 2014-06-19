{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Concurrent.MVar
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as C
import qualified Data.ByteString.Lazy.Char8 as CL
import           Data.Char
import qualified Data.Configurator          as Config
import qualified Data.List                  as L
import           Data.Maybe
import           Data.Time                  (getCurrentTime)
import           Data.Word
import qualified Network                    as HC (withSocketsDo)
import qualified Network.Connection         as HC
import qualified Network.HTTP.Conduit       as HC
import qualified Network.HTTP.Types.Status  as HC
import           Snap
------------------------------------------------------------------------------
import           Application
import qualified Messages.RespAuth          as REA
import qualified Messages.RespFacade        as REF
import qualified Messages.RqAuth            as RQA
import qualified Messages.RqFacade          as RQF
import qualified Model.Contract             as Contract
import qualified Model.Credential           as Credential
import           Model.UUID                 as UUID
import qualified Util.JSONWebToken          as JWT

------------------------------------------------------------------------------
-- | Print the application state.
status :: AppHandler ()
status = do
    contr <- gets _contract
    tokensMVar <- gets _activeTokens
    tokens <- liftIO $ readMVar tokensMVar
    urlAuth <- gets _authURL
    urlFacade <- gets _facadeURL
    let hdr = ["Contract", "Tokens", "Auth Server", "Facade Server"] :: [String]
        str = [show contr, show tokens, show urlAuth, show urlFacade]
    writeBS $ C.intercalate "\n\n" $ fmap (C.pack . show) (zip hdr str)

------------------------------------------------------------------------------
-- | Show usage information.
usage :: Snap ()
usage = writeBS "GET /client/<HTTP Method>/<Service + ServiceParams>"

-- TODO: Verify signature.
fromJWT :: FromJSON a => C.ByteString -> IO (Maybe a)
fromJWT jwtCompact = do
    privKey <- liftM read $ readFile "../jwt-min/data/keys/rsa/sen_key.priv"
    return $ liftM fst $ JWT.decrypt privKey jwtCompact

toJWT :: ToJSON a => a -> IO C.ByteString
toJWT request = do
    privKey <- liftM read $ readFile "../jwt-min/data/keys/rsa/sen_key.priv"
    pubKey <- JWT.serverPubKey
    liftIO $ JWT.signAndEncrypt privKey pubKey request


class FromJSON a => Resp a
instance Resp REF.RespFacade
instance Resp REA.RespAuth
parseResponse :: (Resp a) => HC.Response b -> IO a
parseResponse resp = do
    res <- fromJWT jwtHeaderContents
    maybe (error "Error parsing response.") return res
  where
    jwtHeaderContents =
        let header = L.find (\(h,_) -> h == "JWT") $ HC.responseHeaders resp
        in case header of
                (Just (_, contents)) -> contents
                _        -> error "Header JWT not found."

currentService :: AppHandler UUID
currentService = do
    r <- getRequest
    let maybeService = UUID.fromByteStringSafe' (rqPathInfo r)
    maybe (error "Invalid service UUID!") return maybeService

rqAuth :: AppHandler Token
rqAuth = getToken
    <|> (rqAuth01 >>= processRespAuth01
        >>= rqAuth02 >>= processRespAuth02)
  where
    getToken = do
        serviceUUID <- currentService
        meth <- fromMaybe "GET" <$> getParam "httpMethod"

        tokensMVar <- gets _activeTokens
        tokens <- liftIO $ readMVar tokensMVar
        let maybeToken = L.find (\t -> service t == serviceUUID)
                                tokens
        case maybeToken of
            Just token -> do
                now <- liftIO getCurrentTime
                if expiresAt token > now
                  then if elem (C.map toUpper meth) $ allowedMethods token
                            then return token
                             else pass -- Can't access using the specified method.
                   else do -- Token is expired!
                        liftIO $ modifyMVar_ tokensMVar -- Removes from token list.
                                    (return . filter ((==) serviceUUID . service))
                        pass
            _ -> pass

    execAuthRequest jwt = do
        url <- getUrlPrefix _authURL
        liftIO $ putStrLn $ "URL is: " ++ show url

        manager <- gets _httpMngr
        resp <- liftIO $ HC.withSocketsDo $ do
            initReq <- HC.parseUrl url
            jwtCompact <- toJWT jwt
            let hdrs = ("JWT", jwtCompact) : HC.requestHeaders initReq
                req = initReq { HC.checkStatus = \_ _ _ -> Nothing
                              , HC.requestHeaders = hdrs }
            liftIO $ putStrLn "============ REQUEST ==================="
            liftIO $ print req
            HC.httpLbs req manager
        liftIO $ putStrLn "============ RESPONSE =================="
        liftIO $ print resp
        liftIO $ parseResponse resp

    rqAuth01 = do
        contractUUID <- Contract.uuid <$> gets _contract
        now <- liftIO getCurrentTime
        let request = RQA.RqAuth01 contractUUID now
        prettyWriteJSON "REQ AUTH 01" request
        execAuthRequest request

    processRespAuth01 resp@(REA.RespAuth01 _ credCode chalUUID _) = do
        prettyWriteJSON "RESP AUTH 01" resp
        contr <- gets _contract
        let (Just cred) = L.find (\c -> credCode == Credential.code c)
                                 (Contract.credentials contr)
            credValue    = Credential.value cred
        return (chalUUID, credValue) -- Returns the challenge and its response.
    processRespAuth01 _ = undefined

    rqAuth02 (challengeUUID, credential) = do
        contractUUID <- Contract.uuid <$> gets _contract
        serviceUUID <- currentService
        now <- liftIO getCurrentTime
        let request = RQA.RqAuth02 challengeUUID contractUUID
                                   serviceUUID credential now
        -- DEBUG
        prettyWriteJSON "REQ AUTH 02" request
        execAuthRequest request

    processRespAuth02 resp@(REA.RespAuth02 _ authToken expires) = do
        prettyWriteJSON "RESP AUTH 02" resp
        serviceUUID <- currentService
        let newToken = Token { value = authToken
                             , service = serviceUUID
                             , allowedMethods = ["GET", "POST", "PUT", "DELETE"]
                             , expiresAt = expires }
        tokens <- gets _activeTokens
        liftIO $ modifyMVar_ tokens (return . (newToken :))
        return newToken
    processRespAuth02 _ = undefined

rqFacade :: Token -> AppHandler ()
rqFacade token =
    rqFacade01 >>= processRespFacade01
  where
    getServiceURL = do
        r <- getRequest
        serv <- currentService
        let i = C.concat [ UUID.toByteString' serv
                         , let q = rqQueryString r
                           in if C.null q
                                then ""
                                else C.append "?" q ]
        liftM (++ C.unpack i) $ getUrlPrefix _facadeURL

    rqFacade01 = do
        maybeMethod <- getParam "httpMethod"
        -- TODO: Fix Client/Facade Server to handle uppercase/lowercase
        -- better for HTTP methods.
        let meth = maybe "GET" (C.map toUpper) maybeMethod

        url <- getServiceURL
        liftIO $ putStrLn $ "URL is: " ++ show url

        contractUUID <- Contract.uuid <$> gets _contract

        let request  = RQF.RqFacade01 contractUUID (value token)

        prettyWriteJSON "REQ FACADE" request

        manager <- gets _httpMngr
        resp <- liftIO $ HC.withSocketsDo $ do
            initReq <- HC.parseUrl url
            privKey <- liftM read $ readFile "../jwt-min/data/keys/rsa/sen_key.priv"
            pubKey <- JWT.serverPubKey
            jwtCompact <- liftIO $ JWT.signAndEncrypt privKey pubKey request
            let hdrs = ("JWT", jwtCompact) : HC.requestHeaders initReq
                req = initReq { HC.checkStatus = \_ _ _ -> Nothing
                              , HC.method = meth
                              , HC.requestHeaders = hdrs }
            HC.httpLbs req manager

        if HC.statusCode (HC.responseStatus resp) /= 200 -- Not OK!
            then do
                (resp' :: REF.RespFacade) <- liftIO $ parseResponse resp
                return $ Left resp'
            else return $ Right $ HC.responseBody resp

    processRespFacade01 (Left resp) =
        prettyWriteJSON "RESP FACADE 01 (*ERROR*)" resp
        -- client -- We could call 'client' function here again (maybe the
        -- token expired before arriving at facade server), but we could
        -- fall into infinite loops because currently there's no control of
        -- the number of jumps between auth and facade server.
    processRespFacade01 (Right responseBody) = do
        prettyWrite "SERVICE RESPONSE"
        writeLBS responseBody

------------------------------------------------------------------------------
-- | Client main handler.
client ::AppHandler ()
client = rqAuth >>= rqFacade

------------------------------------------------------------------------------
-- | Util.
-- (App -> String) = _facadeURL or _authURL.
getUrlPrefix :: (App -> String) -> AppHandler String
getUrlPrefix lens = do
    url <- gets lens
    return $ "https://" ++ url ++ "/"

prettyWriteJSON :: (ToJSON a) => String -> a -> Handler App App ()
prettyWriteJSON header v = do
    writeBS $ C.pack $ replicate 30 '-' ++ " " ++ header ++ " " ++ replicate (60 - length header) '-'
    writeBS "\n"
    writeBS $ CL.toStrict (encodePretty v)
    writeBS "\n"

prettyWrite :: String -> Handler App App ()
prettyWrite v = do
    writeBS $ C.pack $ replicate 30 '-' ++ " " ++ v ++ " " ++ replicate (60 - length (show v)) '-'
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
    contr <- liftIO $ fromMaybe (error "Could not parse \'contract\'.json.")
                       <$> decode <$> CL.readFile "resources/contract.json"
    tokens <- liftIO $ newMVar []
    config <- liftIO $ Config.load [Config.Required "resources/devel.cfg"]
    authUrl <- getAuthURL config
    facadeUrl <- getFacadeURL config
    -- According to the documentation at:
    -- (http://hackage.haskell.org/package/http-conduit-2.0.0.7/docs/Network-HTTP-Conduit.html#t:Request), creating a new manager is an expensive operation. So, we create it only once on application start and share it accross all the requests.
    mngr <- liftIO noSSLVerifyManager
    addRoutes routes
    return $ App contr tokens authUrl facadeUrl mngr
  where
    getAuthURL config = do
        host <- liftIO $ Config.lookupDefault "localhost" config "authHost"
        port :: Word16 <- liftIO $ Config.lookupDefault 9000 config "authPort"
        return $ host ++ ":" ++ show port

    getFacadeURL config = do
        host <- liftIO $ Config.lookupDefault "localhost" config "facadeHost"
        port :: Word16 <- liftIO $ Config.lookupDefault 8000 config "facadePort"
        return $ host ++ ":" ++ show port

    noSSLVerifyManager = let tlsSettings = HC.TLSSettingsSimple {
                                -- This is where we disable certificate verification
                                HC.settingDisableCertificateValidation = True,
                                HC.settingDisableSession = False,
                                HC.settingUseServerName  = True }
                         in HC.newManager $ HC.mkManagerSettings tlsSettings Nothing

