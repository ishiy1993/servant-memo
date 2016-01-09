{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either
import Data.Int (Int64)
import Data.Time
import Database.Persist
import Database.Persist.Sql
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Servant

import Api
import Model

main :: IO ()
main = do
    doMigrate
    run 8080 app

app :: Application
app = logStdoutDev $ serve api server

server :: Server API
server = getMemos
    :<|> postMemo
    :<|> getMemoDetail
    :<|> putMemo
    :<|> deleteMemo
        where
            getMemos :: EitherT ServantErr IO [MemoInfo]
            getMemos = do
                memos <- runDb $ selectList [] []
                let mis = map entityToMemoInfo memos
                return mis
            postMemo :: MemoData -> EitherT ServantErr IO ()
            postMemo (MemoData tt cn) = do
                dt <- lift $ getCurrentTime
                let memo = Memo tt dt cn
                runDb $ insert_ memo
            getMemoDetail :: Int64 -> EitherT ServantErr IO Memo
            getMemoDetail id = do
                let key = toSqlKey id
                mm <- runDb $ get key
                case mm of
                  Nothing -> left err404
                  Just m -> return m
            putMemo :: Int64 -> MemoData -> EitherT ServantErr IO Memo
            putMemo id (MemoData tt cn) = do
                let key = toSqlKey id
                dt <- lift $ getCurrentTime
                let newMemo = [ MemoTitle =. tt
                              , MemoDate =. dt
                              , MemoContent =. cn
                              ]
                runDb $ updateGet key newMemo
            deleteMemo :: Int64 -> EitherT ServantErr IO ()
            deleteMemo id = do
                let key = toSqlKey id :: Key Memo
                runDb $ delete key

entityToMemoInfo :: Entity Memo -> MemoInfo
entityToMemoInfo (Entity memoId memo) = 
    MemoInfo (fromSqlKey memoId) (memoTitle memo)
