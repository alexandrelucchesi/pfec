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
import           Snap
import           Snap.Core
import           Snap.Extras.JSON
import           Snap.Extras.CoreUtils
import           Snap.Http.Server.Config
import           Snap.Snaplet
import           Snap.Snaplet.SqliteSimple
import           Snap.Util.FileServe
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
--import           MySnaplets.MyAuth
import qualified Db
import qualified Util.Base64 as B64

------------------------------------------------------------------------------
-- | Service that says hello every time it's called.
hello :: Snap ()
hello = writeBS "Hello!"

------------------------------------------------------------------------------ | Handler that represents the Facade Server. It acts like a front controller, interceptor and filter.
facade :: Handler App App ()
facade = do
    rq <- getRequest
    let rqFacade = do
        info <- getHeader "JWT" rq
        decode . B64.decode . BL.fromStrict $ info
    maybe notAuthorized {-handlerRqFacade-} handlerRqFacade'' rqFacade

notAuthorized :: Handler App App ()
notAuthorized = do
    modifyResponse $ setResponseCode 401 
    r <- getResponse
    finishWith r


------------------------------------------------------------------------------ | Handler that treats requests to Facade Server.
{-
-}
handlerRqFacade'' :: RQF.RqFacade -> Handler App App ()
handlerRqFacade'' rq@(RQF.RqFacade01 _ _) =
    (allow `EXM.catches`
        [ EXM.Handler $ \(_ :: EX.PatternMatchFail) -> pass
        , EXM.Handler $ \(_ :: EX.SomeException)    -> pass ]
    ) <|> liftIO (putStrLn "You have to authenticate first, dude! :/")

  where
    allow = with db $ do
        (Just cred)  <- Db.findCredentialByName (RQF.authCredential rq) 
        (Just user)  <- Db.findUserByCredentialId (fromJust $ Db.credentialCode cred)
        (Just contr) <- Db.findContractByUserId (fromJust $ Db.userCode user) 

        services <- return $ Db.contrServices contr
        service  <- rqServiceURL
        liftIO $ print service -- DEBUG
        if elem service services
            then do
                liftIO $ putStrLn "You're successfully authenticated bro!"
                return ()
            else pass

    rqServiceURL :: Handler App b Db.Service
    rqServiceURL = do -- monadic style
        req    <- getRequest
        header <- return $ getHeader "Host" req
        url    <- return $ header >>= (parseURI . C.unpack . (`C.append` (rqURI req)))
        if isNothing url
            then badReq "Bad Request"
            else return $ Db.Service Nothing (fromJust url)

    rqServiceURL' :: Handler App b Db.Service
    rqServiceURL' = do -- applicative style
        url <- (\v1 v2 -> C.append <$> v1 <*> v2)
                <$> (getHeader "Host" <$> getRequest)
                <*> (pure . rqContextPath <$> getRequest)
        if isNothing url
            then badReq "Bad Request"
            else return $ Db.Service Nothing (fromJust $ url >>= parseURI . C.unpack)


handlerRqFacade' :: RQF.RqFacade -> Handler App App ()
handlerRqFacade' rq@(RQF.RqFacade01 _ _) = with db $ do
    r <- Db.findCredentialByName (RQF.authCredential rq) 
    case r of
        Just cred -> do
            r'' <- Db.findUserByCredentialId (fromJust $ Db.credentialCode cred)
            case r'' of
                Just user -> do
                    r'''<- Db.findContractByUserId (fromJust $ Db.userCode user) 
                    case r''' of
                        Just contr -> do
                            req <- getRequest
                            let services = Db.contrServices contr
                                (Just service) = rqService req
                            if elem service services
                                then return ()
                                else fail "not authorized" -- TODO: HOW THE FUCK DO I CALL notAuthorized FROM HERE?!
                        _ -> error $ errorMsgDb'' (Db.userCode user)
                _ -> error $ errorMsgDb' (Db.credentialCode cred)
        _ -> error errorMsgDb 
  where
    rqService req = do
        h <- getHeader "Host" req
        url <- parseURI . C.unpack $ h `C.append` rqContextPath req
        return $ Db.Service Nothing url
    errorMsgDb = "handlerRqFacade: Could not find credential by name: " ++ show (RQF.authCredential rq)
    errorMsgDb' credId = "handlerRqFacade: Could not find user by credential id: " ++ show credId
    errorMsgDb'' contrId = "handlerRqFacade: Could not find contract by user id: " ++ show contrId

--handlerRqFacade :: RQF.RqFacade -> Handler App App ()
--handlerRqFacade (RQF.RqFacade01 contractCode authCredential) = do
--    r <- query "SELECT cod_contrato, cod_usuario FROM tb_credencial WHERE credencial = ?" (Only authCredential)
--    case r of
--        [(contrCode, userCode) :: (Int, Int)] -> allow <|> genChallenge contrCode userCode 
--        _ -> allow
--
--  where
--    genChallenge :: Int -> Int -> Handler App App ()
--    genChallenge contrCode userCode = do
--        r' <- query "SELECT cod_contrato, cod_usuario, cod_credencial, credencial FROM tb_credencial WHERE cod_contrato = ? AND cod_usuario = ? ORDER BY RANDOM() LIMIT 1;" (contrCode, userCode)
--        case r' of
--            [(contrCode', userCode', credCode, cred) :: (Int, Int, Int, T.Text)] -> do
--                t <- liftIO $ getCurrentTime
--                execute "INSERT INTO tb_desafio VALUES (NULL, ?, ?, ?, ?)" (contrCode', userCode', cred, t)
--                r'' <- query_ "SELECT last_insert_rowid()"
--                case r'' of
--                    [(Only chalCode)] -> do
--                        let authServerURL  = fromJust $ parseURI "http://localhost:8000/auth"
--                            response = REF.RespFacade01 authServerURL chalCode credCode userCode'
--                            
--                        modifyResponse (setHeader "JWT" $ BL.toStrict . B64.encode . encode $ response) -- put response in header. 
--    --                            modifyResponse (setContentType "application/json")
--    --                            writeLBS $ encode response
--                        resp <- getResponse
--                        finishWith resp
--                    _ -> writeBS "FALHOU!"
--            _ -> writeBS "FALHOU!"
--
--    allow :: Handler App App ()
--    allow = do
--        r' <- query "SELECT cod_contrato_servico FROM tb_servico_credencial WHERE credencial_auth = ?" (Only authCredential)
--        case r' of
--            [servContrCode :: [Int]] -> do
--                let queryStr = "SELECT tb_servico.url_servico FROM tb_contrato_servico INNER JOIN tb_servico_credencial ON tb_contrato_servico.cod_contrato_servico = tb_servico_credential.cod_contrato_servico INNER JOIN tb_servico ON tb_contrato_servico.cod_servico = tb_servico.cod_servico"
--                execute_ queryStr -- (Only $ head servContrCode)
--                modifyResponse (addHeader "Debug" (C.pack . show $ authCredential))
--                resp <- getResponse
--                finishWith resp
--                --return ()
--            _ -> do
--                --modifyResponse (addHeader "Debug" (C.pack . show $ authCredential))
--                --resp <- getResponse
--                --finishWith resp
--                empty -- writeBS "FALHOU!"
            

------------------------------------------------------------------------------ | Handler that sends appropriate responses to the client.
--msg05Handler :: Msg -> Handler App App ()
--msg05Handler _ = undefined
-- Concatena login-senha (resposta desafio) e verifica 
-- se a resposta está correta a partir do cod_desafio informado.
-- Se sim
--      - Gera uma credencial nova (texto) e grava na tabela service_credencial com um novo datetime.
--      - Envia para o cliente a credencial.
-- Se não
--      - Usuário não autenticado (código HTTP).


--msg03Handler :: Msg -> Handler App App ()
--msg03Handler (Msg03 cred codChal codContr) =
--    if verifyChallengeCredential cred codChal codContr
--        then do
--            --(codUser, codChalAuth) <- saveChallenge
--            --let resp = Msg04 codChalAuth codUser
--            let resp = Msg04 10 11 
--            writeJSON resp
--        else fail "Else msg03Handler."
            

    -- Insert into database:
    --    cod_contrato
    --    cod_usuario (recupera aleatoriamente a partir de cod_contrato)
    --    resp_desafio (a partir de cod_usuario, obtém "login-senha" [concatenação])
    --    cod_desafio (código de resp_desafio no BD [id])
    --    timestamp (data/hora de gravação do desafio)
    
-- Recupera a credencial na tabela de desafios a partir do código do desafio.

auth :: Handler App App ()
auth = do
    rq <- getRequest
    let rqAuth = do
        info <- getHeader "JWT" rq
        decode . B64.decode . BL.fromStrict $ info
    maybe notAuthorized handlerRqAuth rqAuth
    
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
--                            modifyResponse (setContentType "application/json")
--                            writeLBS $ encode response
                            resp <- getResponse
                            finishWith resp
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
                                    --modifyResponse (setContentType "application/json")
                                    --writeLBS $ encode response
                                    resp <- getResponse
                                    finishWith resp
                                    --writeBS $ C.pack $ "Authenticated!\n\nDiff time is: " ++ show diff
                                _ -> writeBS "CAIU AQUI!!!" >> notAuthorized
                else do
                    writeBS $ C.pack $ "Not Authenticated!\n\nDiff time is: " ++ show diff
                    --notAuthorized -- TODO: Use appropriate HTTP code for timestamp (challenge expired).
        _ -> writeBS "AQUI!"


handlerRqAuth _ = writeBS "Non exhaustive!"
                    

test = do
    req <- getRequest
    let rqUrl = parseURI . C.unpack . (`C.append` rqContextPath req) <$> getHeader "Host" req
    liftIO $ print rqUrl

failService = fail "I always fail! :("
   
------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(C.ByteString, Handler App App ())]
routes = [ ("/facade", facade)
         , ("/auth", auth)
         , ("/hello", liftSnap hello)
         , ("/test", test)
         , ("/fail", failService)
         , ("/", facade)
         ]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "auth-server" "REST-based authentication server." Nothing $ do
--    a   <- nestSnaplet "auth" myAuth myAuthInit
    d <- nestSnaplet "db" db sqliteInit
    addRoutes routes
--    wrapSite (\service -> undirify >> service)
    wrapSite (\service -> facade >> service)
    return $ App d

