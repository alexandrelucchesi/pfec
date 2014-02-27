{-# LANGUAGE OverloadedStrings #-} 

module Messages.HttpResponse where

import Snap.Core
import Snap.Extras

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

