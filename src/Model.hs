{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Model where

import Data.Aeson
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import GHC.Generics

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Memo
    title Text
    date UTCTime
    content Text
    deriving Generic
|]

instance ToJSON Memo
instance FromJSON Memo

data MemoInfo = MemoInfo { id :: Int64
                         , tit :: Text
                         } deriving Generic

instance ToJSON MemoInfo
instance FromJSON MemoInfo

data MemoData = MemoData { tt :: Text
                         , cn :: Text
                         } deriving Generic

instance ToJSON MemoData
instance FromJSON MemoData

db :: Text
db = "memo.db"

doMigrate :: IO ()
doMigrate = runSqlite db $ runMigration migrateAll

runDb query = runSqlite db query
