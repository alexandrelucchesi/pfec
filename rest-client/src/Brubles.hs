{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Brubles where

import           Control.Monad
import qualified Crypto.Padding             as K
import           Data.Aeson
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as C
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.Text.IO               as T

import qualified JWE                        as E
import qualified JWS                        as S
import qualified JWT
import           Util
--import Messages.Types hiding (fromCompactJWT)
import           Messages.RqFacade

main :: IO ()
main = do
    return ()

fromCompactJWT :: FromJSON a => C.ByteString -> IO (Maybe a)
fromCompactJWT jwtContents = do
    myPrivKey   <- liftM read $ readFile "/Users/alexandrelucchesi/Development/haskell/pfec/server-common/resources/keys/rsa-key.priv"
    theirPubKey <- liftM read $ readFile "/Users/alexandrelucchesi/Development/haskell/pfec/server-common/resources/keys/TJDFT/rsa-key.pub"
    JWT.fromCompact myPrivKey theirPubKey jwtContents

--fromCompactJWT :: C.ByteString -> IO C.ByteString
--fromCompactJWT jwtContents = do
--    myPrivKey   <- liftM read $ readFile "/Users/alexandrelucchesi/Development/haskell/pfec/server-common/resources/keys/rsa-key.priv"
--    theirPubKey <- liftM read $ readFile "/Users/alexandrelucchesi/Development/haskell/pfec/server-common/resources/keys/TJDFT/rsa-key.pub"
--    JWT.fromCompact' myPrivKey theirPubKey jwtContents

msg :: C.ByteString
msg = "eyJhbGciOiJSU0ExXzUiLCJlbmMiOiJBMTI4Q0JDLUhTMjU2IiwiY3R5IjoiSldUIn0=.Tcg8UjHmfGdHcsYZIvO-SbYtIVrJNB3QI4IifnAPPPNj38HZyi_IQuBU6qFNqK02ovcVbsWfyc9BSR5u_2YPv5EwjxE1PaK8SGswlpRsqc3517hkSaSZ1sH79FPbLzHU15TOLu7iAcdE3qavTGJJrlZP1Vgmb-aqxal_hUlG-qFqLqv1M1rmocybzLJ6EFVWprhd9VnUaZOMUt6AwBMmYDba-MlyYHk3Ahnha2z9zMHrZ1zgVtQjRvTjYpRbdIUMJHmVmWx-nvHk8ZFp4eh7eg8-AbMQ4jJccYwY1-nNmatHrghZ6EOm9jth-KlZYaBDWCep7IdBFizsb2NWOh3UPQ==.q\253f\136\247\US0\163)\158\128\140\179\152\230\RS.YoUKPqVmOM6kPpC0rAL3xVbqOGhmXx_gbgL3hzc10c_Rxj4X-RjFVwcbDpRtNnQfRuYOqiQ5KWJtorjlxKDxq2a3bNgas5Oep21hqNIenavETKVmigNKBQCr7qxcyWfDvULSmt7XBKuwMQFh4AOmVM08jF_NaPegmG1KlcusvVY3y0Fv-UFBMFioE8k9BAFNdLZLYirI3L4KSvQR6GnPieYyUhtzhNKkFv9GyxoWTK_R3acSZGWRcIDl_EFCpmaQfZj2prBcsWBxHA0INRYTxxBbyT6xkmHlPkp9aBxJG9BBma_T1o7mR1x1731ZSPn48KkzKoX_OfDBDnC7Of5aG43Y0uGYW2XkQziPJBgemmBnUU-9vF0J3oLsD2Ijdy01Sn3hRhd59ob4mrvUYvtW4FWlaNM1zDKb60HEzmFs0HnwQO89GNh079kJdXlvcVaq-wg79-QfRxN2kLe7z0Gclzdmg7ys8oTKB22aIMMPhBaq7EvjnzH6bqCAkQbsb58zZ5mzfq0bx9N2wRst8xHhw9KfvMGs35W92Erg8aG86Id8i3_ChweQ09tD3ahJ0Ty2FLpDWlbZfgL_ebGZ_WqwSFk8M_rinSeK2j-sibGxS7U=.eyp8nKQbGLB8Y4BRrkEgiZkC7KOP7x6VfnLYWkQnZn4="


test :: IO ()
test = do
    -- ENCODE COMPACT JWT
    clientPrivKey <- liftM read $ readFile "resources/rsa-key.priv"
    serverPubKey  <- liftM read $ readFile "resources/rsa-server-key.pub"
    g <- cprg

    let msg = RqFacade01 { contractCode = 1
                         , authCredential = "xyz123"
                         , authorCredential = Just "abc"
                         }
        jwe = S.signJWS clientPrivKey (T.decodeUtf8 . CL.toStrict . encode $ msg)
        jwt = JWT.encrypt g serverPubKey (T.decodeUtf8 jwe)

    C.putStrLn jwt

    -- DECODE COMPACT JWT
--    serverPrivKey <- liftM read $ readFile "/Users/alexandrelucchesi/Development/haskell/pfec/server-common/resources/keys/rsa-key.priv"
--    clientPubKey  <- liftM read $ readFile "/Users/alexandrelucchesi/Development/haskell/pfec/server-common/resources/keys/TJDFT/rsa-key.pub"

--    let (header, jwe') = E.decryptJWE serverPrivKey jwt
--        (Right msg')   = S.verifyJWS clientPubKey (T.encodeUtf8 jwe')
    (res :: Maybe RqFacade) <- fromCompactJWT jwt
--    res <- fromCompactJWT jwt
--    print res

--    C.putStrLn header
--    T.putStrLn msg'
    print res

