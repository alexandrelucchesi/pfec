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
import           Data.Time
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Snap
import           Snap.Extras.CoreUtils
import           Snap.Snaplet.SqliteSimple
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
-- | The application's routes.
routes :: [(C.ByteString, Handler App App ())]
routes = [ ("/facade", facade)
         , ("/add", add)
         , ("/hello", liftSnap hello)
         , ("/", facade)
         ]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "facade-server" "Facade to RESTful web-services." Nothing $ do
    d <- nestSnaplet "db" db sqliteInit
    addRoutes routes
    wrapSite (facade *> isOk *>)
    return $ App d
  where
    isOk = do
        resp <- getResponse
        if rspStatus resp == 200 then return () else finishWith resp

