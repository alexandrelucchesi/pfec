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
import           Crypto.Random
import           Data.Aeson
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.List as L
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import           Data.Maybe
import           Data.Time
import qualified Data.Text as T
import           System.Random
import           Snap
import           Snap.Snaplet.SqliteSimple
------------------------------------------------------------------------------
import           Application
import qualified Messages.RqAuth as RQA
import qualified Messages.RespAuth as REA
import           Messages.HttpResponse
import           Messages.Types
import qualified Model.Challenge as Challenge
import qualified Model.Contract as Contract
import qualified Model.User as User
import qualified BusinessLogic as Db -- TODO: Move all the business' logic code into this module.
import qualified Util.Base64 as B64


------------------------------------------------------------------------------ | Handler that authenticates users.
auth :: Handler App App ()
auth = do
    rq <- getRequest
    --let rqAuth = do
    --        info <- getHeader "JWT" rq
    --        decode . B64.decode . BL.fromStrict $ info
    --maybe unauthorized handlerRqAuth rqAuth
    case getHeader "JWT" rq of
        Just jwtCompact -> do
            (rqAuth :: Maybe RQA.RqAuth) <- liftIO $ fromCompactJWT jwtCompact
            maybe badRequest handlerRqAuth rqAuth
        _ -> badRequest
    
handlerRqAuth :: RQA.RqAuth -> AppHandler ()
handlerRqAuth (RQA.RqAuth01 challengeUUID credentialValue) =
    verifyChallengeCredential >>= generateChallengeAuth >> return ()
  where
    verifyChallengeCredential :: AppHandler Contract.Contract
    verifyChallengeCredential = do
        liftIO $ putStrLn "Verifying credential challenge..."
        contracts <- gets _contracts
        let maybeContract =
                L.find (\c -> any (\chal -> Challenge.uuid chal == challengeUUID
                                            && Challenge.answer chal == credentialValue)
                                  (Contract.challengesCredential c))
                       contracts
        maybe forbidden return maybeContract

    generateChallengeAuth :: (MonadIO m) => Contract.Contract -> m Challenge.ChallengeAuth
    generateChallengeAuth contract = do
        liftIO $ putStrLn "Generating auth challenge..."
        let users = Contract.users contract
        index <- liftIO $ liftM (fst . randomR (0, length users - 1)) newStdGen
        let user = users !! index
        uuid <- liftIO UUID.nextRandom 
        now <- liftIO $ getCurrentTime
        let challenge = Challenge.Challenge {
            Challenge.uuid         = uuid,
            Challenge.answer       = C.concat [User.login user, "-", User.password user],
            Challenge.creationDate = now 
        }
        liftIO $ putStrLn "Challenge is..."
        liftIO $ print challenge
        return challenge



--handlerRqAuth rq@(RQA.RqAuth01 cred chalCode contrCode) = do
--    liftIO $ do
--        putStrLn "Processing auth request..."
--        print rq
--        putStrLn "======================================================"
--    r <- query "SELECT cod_usuario FROM tb_desafio WHERE cod_desafio = ? AND resposta_desafio = ?" (chalCode, cred)
--    case r of
--        [(Only _ :: Only Int)] -> do
--            r' <- query "SELECT cod_usuario, login, password FROM tb_usuario WHERE cod_contrato = ? ORDER BY RANDOM() LIMIT 1;" (Only contrCode)
--            case r' of
--                [(userCode', login, pw) :: (Int, String, String)] -> do
--                    datetime <- liftIO $ getCurrentTime
--                    execute "INSERT INTO tb_desafio VALUES (NULL, ?, ?, ?, ?)" (contrCode, userCode', login ++ "-" ++ pw , datetime)
--                    r'' <- query_ "SELECT last_insert_rowid()"
--                    case r'' of
--                        [(Only chalCode' :: Only Int)] -> do
--                            req <- getRequest
--                            let url = "https://" ++ (C.unpack $ rqServerName req)
--                                      ++ ':' : (show $ rqServerPort req)
--                                      ++ "/auth"
--                                response = REA.RespAuth01 chalCode' userCode' (fromJust $ parseURI url)
--                            --modifyResponse (setHeader "JWT" $ BL.toStrict . B64.encode . encode $ response) -- put response in header. 
--                            resp <- liftIO $ toCompactJWT response
--                            modifyResponse (setHeader "JWT" resp) -- put response in header. 
--                        _ -> writeBS "FALHOU!"
--                _ -> writeBS "FALHOU!"
--        _  -> writeBS "FALHOU!"
--
--handlerRqAuth (RQA.RqAuth02 chalCode login senha) = do
--    let chalResp = login `T.append` "-" `T.append` senha
--    r <- query "SELECT cod_contrato, date_time FROM tb_desafio WHERE cod_desafio = ? AND resposta_desafio = ?" (chalCode, chalResp)
--    case r of
--        [(contrCode, datetime) :: (Int, UTCTime)] -> do
--            datetimeNow <- liftIO $ getCurrentTime
--            let diff = diffUTCTime datetimeNow datetime
--            if diff >= 0 && diff <= 10000 -- 3000 segundos, ideal 10s
--                then do
--                    g <- liftIO $ (newGenIO :: IO SystemRandom)
--                    case genBytes 64 g of
--                        Left _          -> writeBS "GEN: It shouldn't happen :-("
--                        Right (cred, _) -> do
--                            r' <- query "SELECT cod_contrato_servico FROM tb_contrato_servico WHERE cod_contrato = ?" (Only contrCode)
--                            case r' of
--                                (xs :: [[Int]]) -> do
--                                    datetime' <- liftIO $ getCurrentTime
--                                    let datetimeExp = addUTCTime 3600 datetime'
--                                        cred' = CL.toStrict $ B64.encode' cred
--                                    mapM_ (\c -> execute "INSERT INTO tb_servico_credencial VALUES (NULL, ?, ?, ?, ?)" (c, cred', datetime, datetimeExp)) (concat xs)  
--                                    let response = REA.RespAuth02 True cred'
--                                    --modifyResponse (setHeader "JWT" $ BL.toStrict . B64.encode . encode $ response) -- put response in header. 
--                                    resp <- liftIO $ toCompactJWT response
--                                    modifyResponse (setHeader "JWT" resp) -- put response in header. 
--                                _ -> writeBS "CAIU AQUI!!!" >> unauthorized
--                else do
--                    writeBS $ C.pack $ "Not Authenticated!\n\nDiff time is: " ++ show diff
--        _ -> writeBS "AQUI!"


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
    eitherContract <- liftIO $ liftM eitherDecode $ CL.readFile "model.json"
    addRoutes routes
    return $ App (either error (:[]) eitherContract)

