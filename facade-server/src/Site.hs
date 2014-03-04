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
import           Control.Applicative
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C
import           Data.Char (toUpper)
import qualified Data.Configurator as Config
import qualified Network as HC (withSocketsDo)
import qualified Network.HTTP.Conduit as HC
import qualified Network.HTTP.Types.Status as HC
import           Data.Time
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Word
import           Snap
import           Snap.Extras.CoreUtils
import           Snap.Internal.Http.Types
import           Snap.Snaplet.SqliteSimple
import           Snap.Types.Headers
------------------------------------------------------------------------------
import           Application
import qualified Messages.RqFacade as RQF
import qualified Messages.RespFacade as REF
import           Messages.HttpResponse
import           Messages.Types
import qualified BusinessLogic as Db
import qualified Util.Base64 as B64


------------------------------------------------------------------------------ | Handler that represents the Facade Server. It acts like a front controller, interceptor and filter.
facade :: Handler App App ()
facade = do
    rq <- getRequest
    let rqFacade = do
            jwt <- getHeader "JWT" rq
            decode . B64.decode . BL.fromStrict $ jwt :: Maybe RQF.RqFacade
    maybe badRequest handlerRqFacade rqFacade

forward :: Method -> URI -> Handler App b ()
forward meth url = do
    respService <- liftIO $ HC.withSocketsDo $ do 
        initReq <- HC.parseUrl $ show url
        let req = initReq { HC.checkStatus = \_ _ _ -> Nothing
                          , HC.method = methodToStr meth }
        liftIO $ C.putStrLn "Forwarding request..."
        liftIO $ print req
        liftIO $ C.putStrLn "==================================================="
        HC.withManager $ HC.httpLbs req
    let resp = emptyResponse { rspHeaders = fromList $ HC.responseHeaders respService
                             , rspStatus  = HC.statusCode $ HC.responseStatus respService
                             , rspStatusReason  = HC.statusMessage $ HC.responseStatus respService }
    putResponse resp
    writeLBS $ HC.responseBody respService
  where
    methodToStr GET        = "GET"
    methodToStr POST       = "POST"
    methodToStr PUT        = "PUT"
    methodToStr DELETE     = "DELETE"
    methodToStr (Method m) = C.pack . map toUpper . C.unpack $ m
    methodToStr _          = error "Not acceptable method."
                                
------------------------------------------------------------------------------ | Handler that treats requests to Facade Server.
handlerRqFacade :: RQF.RqFacade -> Handler App App ()
handlerRqFacade rq@(RQF.RqFacade01 contrCode authenCred authorCred) =
    allow <|> redirectAuth
  where
    allow = with db $ do
        req  <- getRequest
        let service = rqPathInfo req -- Service identifier

        liftIO $ putStrLn $ "Service requested: " ++ show service
        serviceExists <- Db.serviceExists service
        canAccess <- maybe pass (\cred -> Db.canAccessService (fromIntegral contrCode)
                                    (T.encodeUtf8 cred) service
                                ) authorCred

        liftIO $ putStrLn $ ">> Service exists: " ++ show serviceExists
        liftIO $ putStrLn $ ">> Can access    : " ++ show canAccess

        if isJust authorCred -- An authorization credential was passed.
            then if isJust serviceExists
                    then if canAccess
                            then forward (rqMethod req) $ fromJust $
                                serviceExists >>= \url ->
                                parseURI $ show url ++ 
                                    let q = C.unpack . rqQueryString $ req
                                    in if Prelude.null q
                                            then ""
                                            else '?' : q
                            else forbidden -- Can't access! :-(
                    else notFound "Not found" -- Requested service does not exist!
            else pass

    redirectAuth = do
        let authCredential = RQF.authCredential rq
        r <- query "SELECT cod_contrato, cod_usuario FROM tb_credencial WHERE credencial = ?" (Only authCredential)
        case r of
            [(contrCode, userCode) :: (Int, Int)] -> genChallenge contrCode userCode 
            _ -> unauthorized
      where
        genChallenge :: Int -> Int -> Handler App App ()
        genChallenge contrCode userCode = do
            r' <- query "SELECT cod_contrato, cod_usuario, cod_credencial, credencial FROM tb_credencial WHERE cod_contrato = ? AND cod_usuario = ? ORDER BY RANDOM() LIMIT 1;" (contrCode, userCode)
            case r' of
                [(contrCode', userCode', credCode, cred) :: (Int, Int, Int, T.Text)] -> do
                    t <- liftIO $ getCurrentTime
                    execute "INSERT INTO tb_desafio VALUES (NULL, ?, ?, ?, ?)" (contrCode', userCode', cred, t)
                    r'' <- query_ "SELECT last_insert_rowid()"
                    case r'' of
                        [(Only chalCode)] -> do
                            url <- gets _authServerURL
                            let authURL  = fromJust $ parseURI ("https://" ++ url ++ "/auth")
                                response = REF.RespFacade01 authURL chalCode credCode userCode'
                            modifyResponse (setHeader "JWT" $ BL.toStrict . B64.encode . encode $ response) -- put response in header. 
                            unauthorized
                        _ -> writeBS "FALHOU!"
                _ -> writeBS "FALHOU!" 

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(C.ByteString, Handler App App ())]
routes = [ ("/", facade) ]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "facade-server" "Facade to RESTful web-services." Nothing $ do
    config <- liftIO $ Config.load [Config.Required "resources/devel.cfg"]
    url <- getAuthServerURL config
    d <- nestSnaplet "db" db sqliteInit
    addRoutes routes
    return $ App url d
  where
    getAuthServerURL config = do
        host <- liftIO $ Config.lookupDefault "localhost" config "host" 
        port :: Word16 <- liftIO $ Config.lookupDefault 8000 config "port"
        return $ host ++ ":" ++ show port

