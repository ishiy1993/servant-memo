{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Data.Int (Int64)
import Servant

import Model

type MemoAPI = "memos" :> Get '[JSON] [MemoInfo]
          :<|> "memos" :> ReqBody '[JSON] Memo :> Post '[JSON] ()
          :<|> "memos" :> Capture "id" Int64 :> Get '[JSON] Memo
          :<|> "memos" :> Capture "id" Int64 :> ReqBody '[JSON] Memo :> Put '[JSON] Memo
          :<|> "memos" :> Capture "id" Int64 :> Delete '[JSON] ()

type API = Raw :<|> MemoAPI

api :: Proxy API
api = Proxy
