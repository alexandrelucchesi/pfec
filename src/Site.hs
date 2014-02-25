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
import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Monad.IO.Class
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Configurator as Config
import qualified Data.Configurator.Types as Config
import           Data.Word
import           Data.IORef
import           Data.Maybe
import qualified Data.Text as T
import           Network.HTTP.Conduit
import           Snap
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.SqliteSimple
------------------------------------------------------------------------------
import           Application


------------------------------------------------------------------------------
-- | Client handler.
status :: Handler App App ()
status = do
    urlFacade <- gets _facadeURL
    url <- liftIO $ readMVar urlFacade
    writeBS $ C.pack url

------------------------------------------------------------------------------
-- | Client usage information.
usage :: Snap ()
usage = writeBS "GET /client/<HTTP Method>/<Service + ServiceParams>"


------------------------------------------------------------------------------
-- | Client main handler.
client :: Handler App Sqlite ()
client = do
    m <- getParam "httpMethod"
    i <- getRequest >>= return .rqPathInfo 
    writeBS . C.pack $ show m
    writeBS "\n\n"
    writeBS . C.pack $ show i


------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/status", status)
         , ("/client/:httpMethod", method GET $ with db client)
         , ("/", liftSnap usage)
         ]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "client-proxy" "REST-based client." Nothing $ do
    -- config <- getSnapletUserConfig -- For some reason, it does not work with
    -- cabal sandbox.
    config <- liftIO $ Config.load [Config.Required "resources/devel.cfg"]
    url <- getFacadeURL config
    urlRef <- liftIO $ newMVar url
    sqlite <- nestSnaplet "db" db sqliteInit
    addRoutes routes
    return $ App urlRef sqlite
  where
    getFacadeURL config = do
        host <- liftIO $ Config.lookupDefault "localhost" config "host" 
        port :: Word16 <- liftIO $ Config.lookupDefault 8000 config "port"
        return $ host ++ ":" ++ show port

