{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BusinessLogic where

import           Control.Monad.IO.Class
import           Data.ByteString (ByteString)
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
type Service      = URI

serviceExists :: Service -> Handler App Sqlite Bool
serviceExists service = do
    let queryStr = T.concat . L.intersperse " " $
                            [ "SELECT cod_servico FROM tb_servico"
                            , "WHERE url_servico = ?" ]
    (res :: [Only Int64])  <- query (S.Query queryStr) (Only $ show service)
    return $ not (null res)


canAccessService :: ContractCode -> Credential -> Service -> Handler App Sqlite Bool
canAccessService contract credential service = do
    let queryStr = T.concat . L.intersperse " " $
                            [ "SELECT COUNT(*) FROM tb_servico s"
                            , "INNER JOIN tb_contrato_servico cs"
                            , "INNER JOIN tb_servico_credencial sc"
                            , "ON s.cod_servico = cs.cod_servico AND sc.cod_contrato_servico = cs.cod_contrato_servico"
                            , "AND cs.cod_contrato = ? AND sc.credencial_auth LIKE ?" 
                            , "AND s.url_servico = ? AND sc.datetime_exp > datetime('now')" ]
    [Only status :: Only Int64] <- query (S.Query queryStr) (contract, credential, show service)
    return $ status > 0

