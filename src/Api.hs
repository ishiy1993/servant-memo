{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Data.Int (Int64)
import Servant

import Model

type API = "memos" :> Get '[JSON] [MemoInfo]
      :<|> "memos" :> ReqBody '[JSON] MemoData :> Post '[JSON] ()
      :<|> "memos" :> Capture "id" Int64 :> Get '[JSON] Memo
      :<|> "memos" :> Capture "id" Int64 :> ReqBody '[JSON] MemoData :> Put '[JSON] Memo
      :<|> "memos" :> Capture "id" Int64 :> Delete '[JSON] ()

api :: Proxy API
api = Proxy
