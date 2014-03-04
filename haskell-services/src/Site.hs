{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import           Snap.Core
import           Snap.Snaplet
------------------------------------------------------------------------------
import           Application


------------------------------------------------------------------------------ | Service that adds to numbers.
add :: Snap ()
add = do
    x <- getParam "x"
    y <- getParam "y"
    let res = do
            x' <- x >>= (fst <$>) . C.readInt
            y' <- y >>= (fst <$>) . C.readInt
            return $ x' + y'
    writeBS . C.pack $ "Sum is: " ++ show res
    

------------------------------------------------------------------------------ | Service that adds to numbers.
askGoogle :: Snap ()
askGoogle = redirect "http://www.google.com"


------------------------------------------------------------------------------ | Service that says hello every time it's called. 
hello :: Snap ()
hello = writeBS "Hello!"


------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/add", liftSnap add)
         , ("/askGoogle", liftSnap askGoogle)
         , ("/hello", liftSnap hello)
         ]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    addRoutes routes
    wrapSite (isLocalRequest *>)
    return $ App
  where
    isLocalRequest = do
        req <- getRequest
        liftIO $ C.putStrLn $ "Local address: " `C.append` rqLocalAddr req
        liftIO $ C.putStrLn $ "Remote address: " `C.append` rqRemoteAddr req
        unless (rqLocalAddr req == rqRemoteAddr req) $ do
            modifyResponse (setResponseStatus 301 "Unauthorized")
            resp <- getResponse
            finishWith resp

