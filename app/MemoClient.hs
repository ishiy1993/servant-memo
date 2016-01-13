{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

import Control.Monad.Trans.Either
import Data.Int (Int64)
import Data.Text (Text, pack, unpack)
import Data.Time
import System.Environment
import Servant
import Servant.Client

import Api
import Model

main :: IO ()
main = do
    args <- getArgs
    doReq args

getMemos :: EitherT ServantError IO [MemoInfo]
postMemo :: Memo -> EitherT ServantError IO ()
getMemoDetail :: Int64 -> EitherT ServantError IO Memo
putMemo :: Int64 -> Memo -> EitherT ServantError IO Memo
deleteMemo :: Int64 -> EitherT ServantError IO ()

getMemos :<|> postMemo :<|> getMemoDetail :<|> putMemo :<|> deleteMemo =
    client api' url
        where
            api' = Proxy :: Proxy MemoAPI
            url = BaseUrl Http "localhost" 8080

doReq :: [String] -> IO ()
doReq ["list"] = do
    res <- runEitherT $ getMemos
    case res of
      Left err -> putStrLn $ "Error: " ++ show err
      Right mis -> mapM_ (putStrLn . memoInfoToStr) mis
          where
              memoInfoToStr :: MemoInfo -> String
              memoInfoToStr (MemoInfo id tt) =
                  show id ++ ": " ++ unpack tt
doReq ["add", tt, cn] = do
    dt <- getCurrentTime
    let memo = Memo (pack tt) dt (pack cn)
    res <- runEitherT $ postMemo memo
    case res of
      Left err -> putStrLn $ "Error: " ++ show err
      Right _ -> putStrLn "Success"
doReq ["detail", id] = do
    res <- runEitherT $ getMemoDetail (read id)
    case res of
      Left err -> putStrLn $ "Error: " ++ show err
      Right memo -> displayMemo memo
doReq ["modify", id, tt, cn] = do
    dt <- getCurrentTime
    let memo = Memo (pack tt) dt (pack cn)
    res <- runEitherT $ putMemo (read id) memo
    case res of
      Left err -> putStrLn $ "Error: " ++ show err
      Right memo' -> do
          putStrLn "Change to"
          displayMemo memo'
doReq ["delete", id] = do
    res <- runEitherT $ deleteMemo (read id)
    case res of
      Left err -> putStrLn $ "Error: " ++ show err
      Right _ -> putStrLn "Success"
doReq _ = putStr $ unlines
    [ "Usage:"
    , ""
    , "$ memo-client list"
    , "$ memo-client add (title) (content)"
    , "$ memo-client detail (id)"
    , "$ memo-client modify (id) (title) (content)"
    , "$ memo-client delete (id)"
    ]

displayMemo :: Memo -> IO ()
displayMemo (Memo tt dt cn) = putStr . unlines $
    displayTitle (unpack tt) ++ displayDate dt ++ ["", (unpack cn)]
        where
            displayTitle :: String -> [String]
            displayTitle tt = frame ++ ["  " ++ tt ++ "  "] ++ frame
                where
                    frame :: [String]
                    frame = [take n $ repeat '=']
                    n = 4 + length tt
            displayDate :: UTCTime -> [String]
            displayDate dt = ["posted at " ++ dt']
                where
                    dt' = formatTime defaultTimeLocale "%F %a %X" dt
