module Memo where

import Model exposing (..)
import Debug
import Effects exposing (Effects, Never)
import Json.Decode as Json exposing ((:=))
import Http
import Html exposing (..)
import Task

type alias Model =
    { memoInfos : List MemoInfo }

type Action =
    GetMemos (Maybe (List MemoInfo))

init : (Model, Effects Action)
init =
    ( Model []
    , getMemos
    )

update : Action -> Model -> (Model, Effects Action)
update action model =
    case action of
        GetMemos ms ->
            ( {model | memoInfos = (Maybe.withDefault [] ms) }
            , Effects.none
            )

getMemos : Effects Action
getMemos =
    Http.get mis "http://localhost:8080/memos"
        |> Task.toMaybe
        |> Task.map GetMemos
        |> Effects.task

mi : Json.Decoder MemoInfo
mi =
    Json.object2 MemoInfo
        ("tit" := Json.string)
        ("id" := Json.int)

mis : Json.Decoder (List MemoInfo)
mis =
    Json.list mi

memoInfoRow : MemoInfo -> Html
memoInfoRow mi' =
    tr [] [
        td [] [text (toString mi'.id)]
      , td [] [text mi'.tit]
    ]

view : Signal.Address Action -> Model -> Html
view address model =
    div [] [
        h1 [] [text "Memos"]
      , table [] [
          thead [] [
              tr [] [
                  th [] [text "ID"]
                , th [] [text "Title"]
              ]
          ]
        , tbody [] (List.map memoInfoRow model.memoInfos)
        ]
    ]
