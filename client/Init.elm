module Init exposing (..)

import String
import Window
import Task exposing (perform)
import Navigation exposing (Location)

import Utils exposing (nop)
import Types exposing (..)

init : String -> (Model, Cmd Msg)
init hash =
    ( { id = -1
      , shape = []
      , size = { width = 500, height = 500 }
      , moving = False
      , server = String.dropLeft 1 hash
      , clients = []
      , ext = { onHoverServer = False, onHoverClear = False, showServerModal = False, transientServer = "" }
      }
    , perform Error Window Window.size
    )

urlHashParser : Location -> String
urlHashParser location = location.hash

urlUpdate : String -> Model -> (Model, Cmd Msg)
urlUpdate hash model =
    ( { model | server = String.dropLeft 1 hash }, nop )
