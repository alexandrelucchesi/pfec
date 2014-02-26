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
import qualified Control.Monad.CatchIO as EXM
import           Control.Monad.IO.Class
import           Control.Lens
import           Control.Lens.TH
import           Control.Monad
import           Crypto.Random
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL
import           Data.Time
import           Data.Typeable
import           Data.Maybe
import           Data.IORef
import           Data.List
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as TL
import qualified Safe
import           Snap
import           Snap.Core
import           Snap.Extras.CoreUtils
import           Snap.Extras.JSON
import           Snap.Http.Server.Config
import           Snap.Snaplet
import           Snap.Snaplet.SqliteSimple
import           Snap.Util.FileServe
import qualified Snap.Test as ST
import           System.Process
import           System.Random
------------------------------------------------------------------------------
import           Application
import qualified Control.Exception.Base as EX
import qualified Messages.RqAuth as RQA
import qualified Messages.RqFacade as RQF
import qualified Messages.RespAuth as REA
import qualified Messages.RespFacade as REF
import           Messages.Types
import qualified Db
import qualified Util.Base64 as B64

------------------------------------------------------------------------------
-- UTIL
------------------------------------------------------------------------------
-- For clarifications on HTTP status' codes, see:
-- http://stackoverflow.com/questions/8389253/correct-http-status-code-for-resource-which-requires-authorization
badRequest :: MonadSnap m => m b
badRequest = badReq "Bad request"

forbidden :: MonadSnap m => m ()
forbidden = modifyResponse (setResponseCode 403)

--notFound :: MonadSnap m => m ()
--notFound = modifyResponse (setResponseCode 404)

unauthorized :: MonadSnap m => m ()
unauthorized = modifyResponse (setResponseCode 302)

ok :: MonadSnap m => m ()
ok = modifyResponse (setResponseCode 200)

------------------------------------------------------------------------------ | Handler that represents the Facade Server. It acts like a front controller, interceptor and filter.
------------------------------------------------------------------------------
-- FACADE
------------------------------------------------------------------------------
------------------------------------------------------------------------------ | Handler that represents the Facade Server. It acts like a front controller, interceptor and filter.
facade :: Handler App App ()
facade = do
    rq <- getRequest
    let pathInfo = rqPathInfo rq
    if C.isPrefixOf "auth" pathInfo -- TEMPORARY WORKAROUND: LET AUTH'S REQUESTS PASS.
        then auth
        else do
            let rqFacade = do
                info <- getHeader "JWT" rq
                decode . B64.decode . BL.fromStrict $ info :: Maybe RQF.RqFacade
            maybe badRequest handlerRqFacade rqFacade


------------------------------------------------------------------------------ | Handler that treats requests to Facade Server.
handlerRqFacade :: RQF.RqFacade -> Handler App App ()
handlerRqFacade rq@(RQF.RqFacade01 contrCode authenCred authorCred) =
    allow <|> redirectAuth
  where
    allow = with db $ do
        req    <- getRequest
        header <- return $ getHeader "Host" req
        url    <- return $ header >>= (parseURI . C.unpack . (`C.append` (rqURI req)))
        service <- maybe badRequest return url

        liftIO $ putStrLn $ "Service requested: " ++ show service
        serviceExists <- Db.serviceExists service
        canAccess <- maybe pass (\cred -> Db.canAccessService (fromIntegral contrCode)
                                    (T.encodeUtf8 cred) service
                                ) authorCred

        if isJust authorCred -- An authorization credential was passed.
            then if serviceExists
                    then if canAccess
                            then ok -- OK!
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
                            let authServerURL  = fromJust $ parseURI "http://localhost:8000/auth"
                                response = REF.RespFacade01 authServerURL chalCode credCode userCode'
                            modifyResponse (setHeader "JWT" $ BL.toStrict . B64.encode . encode $ response) -- put response in header. 
                            --modifyResponse (setResponseCode 301)
                            unauthorized
                        _ -> writeBS "FALHOU!"
                _ -> writeBS "FALHOU!"


------------------------------------------------------------------------------
-- AUTH
------------------------------------------------------------------------------
------------------------------------------------------------------------------ | Handler that authenticates users.
auth :: Handler App App ()
auth = do
    rq <- getRequest
    let rqAuth = do
        info <- getHeader "JWT" rq
        decode . B64.decode . BL.fromStrict $ info
    maybe unauthorized handlerRqAuth rqAuth
    
handlerRqAuth :: RQA.RqAuth -> Handler App App ()
handlerRqAuth (RQA.RqAuth01 cred chalCode contrCode) = do
    r <- query "SELECT cod_usuario FROM tb_desafio WHERE cod_desafio = ? AND resposta_desafio = ?" (chalCode, cred)
    case r of
        [(Only _ :: Only Int)] -> do
            r' <- query "SELECT cod_usuario, login, password FROM tb_usuario WHERE cod_contrato = ? ORDER BY RANDOM() LIMIT 1;" (Only contrCode)
            case r' of
                [(userCode', login, pw) :: (Int, String, String)] -> do
                    datetime <- liftIO $ getCurrentTime
                    execute "INSERT INTO tb_desafio VALUES (NULL, ?, ?, ?, ?)" (contrCode, userCode', login ++ "-" ++ pw , datetime)
                    r'' <- query_ "SELECT last_insert_rowid()"
                    case r'' of
                        [(Only chalCode' :: Only Int)] -> do
                            let response = REA.RespAuth01 chalCode' userCode'
                            modifyResponse (setHeader "JWT" $ BL.toStrict . B64.encode . encode $ response) -- put response in header. 
                            --resp <- getResponse
                            --finishWith resp
                        _ -> writeBS "FALHOU!"
                _ -> writeBS "FALHOU!"
        _  -> writeBS "FALHOU!"

handlerRqAuth (RQA.RqAuth02 chalCode login senha) = do
    let chalResp = login `T.append` "-" `T.append` senha
    r <- query "SELECT cod_contrato, date_time FROM tb_desafio WHERE cod_desafio = ? AND resposta_desafio = ?" (chalCode, chalResp)
    case r of
        [(contrCode, datetime) :: (Int, UTCTime)] -> do
            datetimeNow <- liftIO $ getCurrentTime
            let diff = diffUTCTime datetimeNow datetime
            if diff >= 0 && diff <= 10000 -- 3000 segundos, ideal 10s
                then do
                    g <- liftIO $ (newGenIO :: IO SystemRandom)
                    case genBytes 64 g of
                        Left _          -> writeBS "GEN: It shouldn't happen :-("
                        Right (cred, _) -> do
                            r' <- query "SELECT cod_contrato_servico FROM tb_contrato_servico WHERE cod_contrato = ?" (Only contrCode)
                            case r' of
                                (xs :: [[Int]]) -> do
                                    datetime' <- liftIO $ getCurrentTime
                                    let datetimeExp = addUTCTime 3600 datetime'
                                        cred' = CL.toStrict $ B64.encode' cred
                                    mapM_ (\c -> execute "INSERT INTO tb_servico_credencial VALUES (NULL, ?, ?, ?, ?)" (c, cred', datetime, datetimeExp)) (concat xs)  
                                    let response = REA.RespAuth02 True cred'
                                    modifyResponse (setHeader "JWT" $ BL.toStrict . B64.encode . encode $ response) -- put response in header. 
                                    --resp <- getResponse
                                    --finishWith resp
                                _ -> writeBS "CAIU AQUI!!!" >> unauthorized
                else do
                    writeBS $ C.pack $ "Not Authenticated!\n\nDiff time is: " ++ show diff
        _ -> writeBS "AQUI!"


handlerRqAuth _ = writeBS "Non exhaustive!"
                    
------------------------------------------------------------------------------
-- SERVICES
------------------------------------------------------------------------------

------------------------------------------------------------------------------ | Service that says hello every time it's called. 
hello :: Snap ()
hello = writeBS "Hello!"


------------------------------------------------------------------------------ | Service that adds to numbers.
add :: Handler App App ()
add = do
    x <- getParam "x"
    y <- getParam "y"
    res <- return $ do
        x' <- x >>= (fst <$>) . C.readInt
        y' <- y >>= (fst <$>) . C.readInt
        return $ x' + y'
    writeBS . C.pack $ "\n\nSum is: " ++ show res
    

------------------------------------------------------------------------------
-- CLIENT
------------------------------------------------------------------------------
------------------------------------------------------------------------------
class FromJSON a => Resp a
instance Resp REF.RespFacade
instance Resp REA.RespAuth

------------------------------------------------------------------------------ | Converts value to JWT encoded.
toJWT :: (ToJSON a) => a -> C.ByteString
toJWT = CL.toStrict . B64.encode . encode

------------------------------------------------------------------------------ | Parses response.
parseResponse :: (Resp a) => Maybe String -> Response -> a
parseResponse msg resp =
    let res = eitherDecode . B64.decode $ jwtHeaderContents 
    in either (error . (++) (if isJust msg then fromJust msg ++ "\n" else "")) id res
  where
    jwtHeaderContents =
        let header = getHeader "JWT" resp 
        in case header of
                (Just v) -> CL.fromStrict v
                _        -> error $ (++) (if isJust msg then fromJust msg ++ "\n" else "") "Header JWT not found."

------------------------------------------------------------------------------ | Requests Facade a service.
-- TODO: Unify rqFacade01 e rqFacade02 here!
rqFacade01 :: Handler App App REF.RespFacade
rqFacade01 = do
    modifyRequest (\req -> setHeader "JWT" invalidToken req)
--  modify (\req -> setHeader "Host" "http://localhost:8000" req)

    -- DEBUG
    prettyWriteJSON "REQ FACADE 01" request

    resp <- facade >> getResponse
    return $ parseResponse (return errorMsg) resp
  where
    request = RQF.RqFacade01 { RQF.contractCode = 1
                             , RQF.authCredential = "xyz123"
                             , RQF.authorCredential = Nothing }
    invalidToken = toJWT request
    errorMsg = "Could not parse RespFacade01."


------------------------------------------------------------------------------ | Requests Facade a service.
rqFacade02 :: REA.RespAuth -> Handler App App (Either REF.RespFacade ())
rqFacade02 rq@(REA.RespAuth02 _ _) =
    if not (REA.isAuthenticated rq)
        then error errorMsg
        else do
            modifyRequest (\req -> setHeader "JWT" token req)

            -- DEBUG
            prettyWriteJSON "REQ FACADE 02" request

            resp <- facade >> getResponse

            if rspStatus resp == 302 -- redirect
                then liftIO $ print $ (parseResponse (return errorMsg) resp :: REF.RespFacade)
                else return ()

            return $ if rspStatus resp == 302 -- redirect
                then Left $ parseResponse (return errorMsg) resp
                else Right $ ()

  where
    request = RQF.RqFacade01 { RQF.contractCode   = 1
                             , RQF.authCredential = "xyz123"
                             , RQF.authorCredential = Just $ T.decodeUtf8 $ REA.credential rq }
    token = toJWT request
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

    modifyRequest (\req -> setHeader "JWT" (toJWT rqAuth) req)

    -- DEBUG
    prettyWriteJSON "REQ AUTH 01" rqAuth

    resp <- auth >> getResponse

    return $ parseResponse (return errorMsg') resp
  where
    errorMsg = "rqAuth01: Could not find credential: " ++ show (REF.credentialCode rq)
    errorMsg' = "rqAuth01: Could not find credential: " ++ show (REF.credentialCode rq)

rqAuth01 _ = error "rqAuth01: It shouldn't happen! :-("

------------------------------------------------------------------------------ | Sends to Auth server the requested user credentials (login || password).
rqAuth02 :: REA.RespAuth -> Handler App App REA.RespAuth
rqAuth02 rq@(REA.RespAuth01 _ _) = do
    r <- with db $ Db.findUserById (fromIntegral $ REA.userCode rq)
    case r of
        Just u -> do
            let rqAuth = RQA.RqAuth02 { RQA.challengeAuthCode = REA.challengeAuthCode rq
                                      , RQA.login = Db.userLogin u
                                      , RQA.password = Db.userPassword u }
            modifyRequest (\req -> setHeader "JWT" (toJWT rqAuth) req)

            -- DEBUG
            prettyWriteJSON "REQ AUTH 02" rqAuth

            resp <- auth >> getResponse
            return $ parseResponse (return errorMsg) resp
        _ -> error errorMsgDb
  where
    errorMsgDb = "rqAuth02: Could not find user with id: " ++ show (REA.userCode rq)
    errorMsg = "rqAuth02: Could not parse RespAuth02."

rqAuth02 _ = error "rqAuth02: It shouldn't happen! :-("

client :: Handler App App ()
client = do
    -- Change request URI.
    modifyRequest (\rq ->
        let stripClient = C.dropWhile (/= '/') . C.tail
            newURI     = stripClient $ rqURI rq
            newCtxPath = stripClient $ rqContextPath rq 
        in rq { rqURI = newURI, rqContextPath = newCtxPath })

    respF01 <- rqFacade01
    prettyWriteJSON "RESP FACADE 01" respF01
    respA01 <- rqAuth01 respF01
    prettyWriteJSON "RESP AUTH 01" respA01
    respA02 <- rqAuth02 respA01
    prettyWriteJSON "RESP AUTH 02" respA02
    respF02 <- rqFacade02 respA02
--    either (prettyWriteJSON "RESP FACADE 02 (*ERROR*)") (\_ -> return ()) respF02
    case respF02 of
        Left v -> (prettyWriteJSON "RESP FACADE 02 (*ERROR*)" v)
        _ -> return ()
    prettyWrite ("END OF CLIENT" :: String)
    writeBS "\n\n"
    routeLocal routes

prettyWriteJSON :: (ToJSON a) => String -> a -> Handler App App ()
prettyWriteJSON header v = do
    writeBS $ C.pack $ (take 30 $ repeat '-') ++ header ++ (take (60 - length header) $ repeat '-')
    writeBS "\n"
    writeBS $ CL.toStrict (encodePretty v)
    writeBS "\n"

prettyWrite :: (Show a) => a -> Handler App App ()
prettyWrite v = do
    writeBS $ C.pack $ (take 30 $ repeat '-') ++ show v ++ (take (60 - length (show v)) $ repeat '-')
    writeBS "\n"

------------------------------------------------------------------------------
-- APPLICATION
------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(C.ByteString, Handler App App ())]
routes = [ ("/facade", facade)
         , ("/add", add)
         , ("/auth", auth)
--         , ("/client", client)
         , ("/hello", liftSnap hello)
         , ("/", facade)
         ]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "auth-server" "REST-based authentication server." Nothing $ do
    d <- nestSnaplet "db" db sqliteInit
    addRoutes routes
    --wrapSite ((bypass <|> facade) *>)
    wrapSite (facade *> isOk *>)
    return $ App d
  where
    isOk = do
        resp <- getResponse
        if rspStatus resp == 200 then return () else finishWith resp
--    bypass = do
--        pathArg (\(str :: C.ByteString) -> if str == "client" then return () else pass)
        --req <- getRequest
        --let status = ((==) "client" <$>) . Safe.headMay
        --             . C.split '/' . rqPathInfo $ req
        --maybe pass (\s -> if s then return () else pass) status


