module Main where

import Model exposing (..)
import Memo exposing (init, update, view, Model)
import Effects exposing (Never)
import Html exposing (Html)
import Task
import StartApp

app : StartApp.App Model
app =
    StartApp.start
        { init = init
        , update = update
        , view = view
        , inputs = []
        }

main : Signal Html
main = 
    app.html

port tasks : Signal (Task.Task Never ())
port tasks =
    app.tasks
