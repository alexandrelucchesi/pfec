{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BusinessLogic where

import           Data.ByteString.Char8 (ByteString, unpack)
import           Data.Int
import qualified Data.List as L
import qualified Data.Text as T
import           Snap.Snaplet
import           Snap.Snaplet.SqliteSimple

import qualified Database.SQLite.Simple as S

import           Application
import           Messages.Types

type ContractCode = Int64
type Credential   = ByteString
type Service      = ByteString -- TODO: Include more info (HTTP method, etc)

serviceExists :: Service -> Handler App Sqlite (Maybe URI)
serviceExists service = do
    let queryStr = T.concat . L.intersperse " " $
                            [ "SELECT host, port, servico FROM tb_servico"
                            , "WHERE url_servico = ?" ]
    res <- query (S.Query queryStr) (Only $ unpack service)
    case res of
        [(host, port, service') :: (String, Int64, String)] ->
            let url = concat ["http://", host, ":", show port, "/", service']
            in return $ parseURI url
        _ -> return Nothing


canAccessService :: ContractCode -> Credential -> Service -> Handler App Sqlite Bool
canAccessService contract credential service = do
    let queryStr = T.concat . L.intersperse " " $
                            [ "SELECT COUNT(*) FROM tb_servico s"
                            , "INNER JOIN tb_contrato_servico cs"
                            , "INNER JOIN tb_servico_credencial sc"
                            , "ON s.cod_servico = cs.cod_servico AND sc.cod_contrato_servico = cs.cod_contrato_servico"
                            , "AND cs.cod_contrato = ? AND sc.credencial_auth LIKE ?" 
                            , "AND s.url_servico = ? AND sc.datetime_exp > datetime('now')" ]
    [Only status :: Only Int64] <- query (S.Query queryStr) (contract, credential, unpack service)
    return $ status > 0

