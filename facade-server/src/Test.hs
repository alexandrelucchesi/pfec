{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as C (map)
import           Data.Char             (toUpper)
import qualified Data.Map              as M
import qualified Messages.RqFacade     as RQF
import qualified Messages.RespFacade   as REF
import           Site
import qualified Snap.Internal.Http.Types as SIT
import           Snap.Snaplet.Test
import qualified Snap.Test             as ST

import qualified Model.UUID            as UUID
import           Util.JSONWebToken

type ContractUUID = UUID.UUID
type AuthorizationToken = ByteString
type Method = ByteString
type Service = ByteString
testRqFacade01 :: ContractUUID -> AuthorizationToken -> Method -> Service -> IO ()
testRqFacade01 contractUUID authorizationToken method service = do
    privKey <- liftM read $ readFile "../jwt-min/data/keys/rsa/sen_key.priv"
    pubKey <- serverPubKey
    let rqBuilder = do
            case C.map toUpper method of
                "POST"   -> ST.postUrlEncoded service M.empty
                "PUT"    -> ST.put service "application/json" "{}"
                "DELETE" -> ST.delete service M.empty
                _        -> ST.get service M.empty
            req <- liftIO $ signAndEncrypt privKey pubKey request 
            ST.addHeader "JWT" req 
            ST.setSecure True   -- Enables HTTPs
    resp <- runHandler message rqBuilder facade app
    either print
           (\r -> do
                putStrLn $ replicate 80 '-'
                ST.dumpResponse r                
                putStrLn $ replicate 80 '-'
                let (r' :: Maybe REF.RespFacade) = liftM fst $
                        decrypt privKey =<< SIT.getHeader "JWT" r
                print r'
           ) resp 
  where
    message = Just "Requests to Facade Server providing the contract code and an authorization token."
    request = RQF.RqFacade01 {
                    RQF.contractUUID       = contractUUID,
                    RQF.authorizationToken = authorizationToken
              }

