module Model where

import Date exposing (Date)

type alias Memo =
    { title : String
    , date : Date
    , content : String
    }

type alias MemoInfo =
    { tit : String
    , id : Int
    }

