{-# LANGUAGE OverloadedStrings #-}

module Db where

import           Control.Applicative
import           Control.Monad
import qualified Data.Text as T
import           Data.Time (UTCTime)
import qualified Database.SQLite.Simple as S
import           Snap.Snaplet
import           Snap.Snaplet.SqliteSimple

import           Application

--data Credential = Credential {
--    credContractCode  :: Int,
--    credCodUser       :: Int,
--    credCodCredential :: Int,
--    credCredential :: T.Text
--} deriving (Eq, Show)

data Contract = Contract {
    contrContractCode :: Int,
    contrName         :: T.Text, -- unique
    contrDescription  :: Maybe T.Text,
    contrCreationDt   :: T.Text
--    contrCreationDt   :: UTCTime
} deriving (Eq, Show)

instance FromRow Contract where
    fromRow = Contract <$> field
                       <*> field
                       <*> field
                       <*> field

listContracts :: Handler App Sqlite [Contract]
listContracts =
    query_ "SELECT * FROM tb_contrato"

insertContractTest :: Handler App Sqlite ()
insertContractTest =
    execute_ "INSERT INTO tb_contrato VALUES(NULL, 'DETRAN', 'DETRAN-DF', '13-02-2014')" 


















