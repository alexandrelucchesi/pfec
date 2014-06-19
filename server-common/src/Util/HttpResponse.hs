{-# LANGUAGE Trustworthy #-} 
{-# LANGUAGE OverloadedStrings #-} 

module Util.HttpResponse where

import Snap.Core
import Snap.Extras

-- For clarifications on HTTP status' codes, see:
-- http://stackoverflow.com/questions/8389253/correct-http-status-code-for-resource-which-requires-authorization
badRequest :: MonadSnap m => m b
badRequest = badReq "Bad request"

forbidden :: MonadSnap m => m a
forbidden = do
    modifyResponse (setResponseCode 403)
    r <- getResponse
    finishWith r

--notFound :: MonadSnap m => m ()
--notFound = modifyResponse (setResponseCode 404)

unauthorized :: MonadSnap m => m a
unauthorized = do
    modifyResponse (setResponseCode 302)
    r <- getResponse
    finishWith r

ok :: MonadSnap m => m ()
ok = modifyResponse (setResponseCode 200)

