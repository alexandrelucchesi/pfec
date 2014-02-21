{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE FlexibleInstances #-}

module Client where

import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Int
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as TL
import           Snap
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.SqliteSimple
import           Snap.Snaplet.Test
import qualified Snap.Test as ST
-----------------------------------------------
import Application
import Site
import qualified Db
import qualified Util.Base64 as B64
import qualified Messages.RqFacade as RQF
import qualified Messages.RespFacade as REF
import qualified Messages.RqAuth as RQA
import qualified Messages.RespAuth as REA

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
rqFacade01 :: IO REF.RespFacade
rqFacade01 = do
    let rqBuilder = do
          ST.get "/hello" M.empty
          modify (\req -> setHeader "JWT" invalidToken req)
          modify (\req -> setHeader "Host" "http://localhost:8000" req)
    resp <- runHandler Nothing rqBuilder facade app
    either (error . show) (return . parseResponse (return errorMsg)) resp
  where
    invalidToken = toJWT RQF.RqFacade01 { RQF.contractCode = 1
                                        , RQF.authCredential = "xyz123" }
    errorMsg = "Could not parse RespFacade01."

-- GENERAL
-- TODO: Unify rqFacade01 e rqFacade02 here!
rqFacade :: IO Response
rqFacade = do
    let rqBuilder = do
          ST.get "/hello" M.empty
          modify (\req -> setHeader "JWT" token req)
          modify (\req -> setHeader "Host" "http://localhost:8000" req)
    resp <- runHandler Nothing rqBuilder facade app
    either (error . show) return {- (ST.getResponseBody) -} resp
  where
    token = toJWT RQF.RqFacade01 { RQF.contractCode = 1
                                 , RQF.authCredential = "xyz123" }

------------------------------------------------------------------------------ | Requests Facade a service.
rqFacade02 :: REA.RespAuth -> IO Response
rqFacade02 rq@(REA.RespAuth02 _ _) =
    if not (REA.isAuthenticated rq)
        then error errorMsg
        else do
            let rqBuilder = do
                  ST.get "/hello" M.empty
                  modify (\req -> setHeader "JWT" token req)
            resp <- runHandler Nothing rqBuilder facade app
            either (error . show) return {- (ST.getResponseBody) -} resp

  where
    token = toJWT RQF.RqFacade01 { RQF.contractCode   = 1
                                 , RQF.authCredential = T.decodeUtf8 $ REA.credential rq }
    errorMsg = "rqFacade02: Could not authenticate user."

rqFacade02 _ = error "rqFacade02: It shouldn't happen! :-("

------------------------------------------------------------------------------ | Requests Authentication providing specified info.
rqAuth01 :: REF.RespFacade -> IO REA.RespAuth
rqAuth01 rq@(REF.RespFacade01 _ _ _ _) = do
    r <- evalHandler Nothing (return ()) (with db $ Db.findCredentialById $ fromIntegral (REF.credentialCode rq)) app
    let maybeCred = either (error . show) id r
        cred = if isJust maybeCred then fromJust maybeCred else error errorMsg
        rqAuth = RQA.RqAuth01 { RQA.credential = Db.credentialCred cred
                              , RQA.challengeCredentialCode = REF.challengeCredentialCode rq
                              , RQA.contractCode = 1 }
    let rqBuilder = do
          ST.get "/auth" M.empty
          modify (\req -> setHeader "JWT" (toJWT rqAuth) req)
    resp <- runHandler Nothing rqBuilder auth app
    either (error . show) (return . parseResponse (return errorMsg')) resp
  where
    errorMsg = "rqAuth01: Could not find credential: " ++ show (REF.credentialCode rq)
    errorMsg' = "rqAuth01: Could not find credential: " ++ show (REF.credentialCode rq)

rqAuth01 _ = error "rqAuth01: It shouldn't happen! :-("

------------------------------------------------------------------------------ | Sends to Auth server the requested user credentials (login || password).
rqAuth02 :: REA.RespAuth -> IO REA.RespAuth
rqAuth02 rq@(REA.RespAuth01 _ _) = do
    r <- evalHandler Nothing (return ()) (with db $ Db.findUserById (fromIntegral $ REA.userCode rq)) app
    let maybeUser = either (error . show) id r
    case maybeUser of
        Just u -> do
            rqBuilder <- return $ do
                ST.get "/auth" M.empty
                let rqAuth = RQA.RqAuth02 { RQA.challengeAuthCode = REA.challengeAuthCode rq
                                          , RQA.login = Db.userLogin u
                                          , RQA.password = Db.userPassword u }
                modify (\req -> setHeader "JWT" (toJWT rqAuth) req)
            resp <- runHandler Nothing rqBuilder auth app
            either (error . show) (return . parseResponse (return errorMsg)) resp
        _ -> error errorMsgDb
  where
    errorMsgDb = "rqAuth02: Could not find user with id: " ++ show (REA.userCode rq)
    errorMsg = "rqAuth02: Could not parse RespAuth02."

rqAuth02 _ = error "rqAuth02: It shouldn't happen! :-("

test :: IO ()
test = do
    respF01 <- rqFacade01
    prettyPrint "RESP FACADE 01" respF01
    respA01 <- rqAuth01 respF01
    prettyPrint "RESP AUTH 01" respA01
    respA02 <- rqAuth02 respA01
    prettyPrint "RESP AUTH 02" respA02
    respF02 <- rqFacade02 respA02
    prettyPrint "RESP FACADE 02" respF02

  where
    prettyPrint header v = do
        putStrLn $ (take 20 $ repeat '-') ++ header ++ (take (60 - length header) $ repeat '-')
        print v


------------------------------------------------------------------------------ | Test DB functions.
testFindCredentialById :: Int64 -> IO Db.Credential
testFindCredentialById credId = do
    let rqBuilder = ST.get "/hello" M.empty
    resp <- evalHandler Nothing rqBuilder (with db $ Db.findCredentialById credId) app
    either (error . show) (\r -> return $ if isJust r then fromJust r else error "Credential not found." ) resp

testFindUserById :: Int64 -> IO Db.User
testFindUserById userId = do
    let rqBuilder = ST.get "/hello" M.empty
    resp <- evalHandler Nothing rqBuilder (with db $ Db.findUserById userId) app
    either (error . show) (\r -> return $ if isJust r then fromJust r else error "User not found." ) resp

testListCredentialsByUserId :: Int64 -> IO [Db.Credential]
testListCredentialsByUserId userId = do
    let rqBuilder = ST.get "/hello" M.empty
    resp <- evalHandler Nothing rqBuilder (with db $ Db.listCredentialsByUserId userId) app
    either (error . show) return resp

testListUsersByContractId :: Int64 -> IO [Db.User]
testListUsersByContractId contrId = do
    let rqBuilder = ST.get "/hello" M.empty
    resp <- evalHandler Nothing rqBuilder (with db $ Db.listUsersByContractId contrId) app
    either (error . show) return resp

testListServicesByContractId :: Int64 -> IO [Db.Service]
testListServicesByContractId contrId = do
    let rqBuilder = ST.get "/hello" M.empty
    resp <- evalHandler Nothing rqBuilder (with db $ Db.listServicesByContractId contrId) app
    either (error . show) return resp




