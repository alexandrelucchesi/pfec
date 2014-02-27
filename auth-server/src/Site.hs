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
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL
import           Data.Time
import qualified Data.Text as T
import           Snap
import           Snap.Snaplet.SqliteSimple
------------------------------------------------------------------------------
import           Application
import qualified Messages.RqAuth as RQA
import qualified Messages.RespAuth as REA
import           Messages.HttpResponse
import qualified BusinessLogic as Db
import qualified Util.Base64 as B64


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
    sqlite <- nestSnaplet "db" db sqliteInit
    addRoutes routes
    return $ App sqlite

