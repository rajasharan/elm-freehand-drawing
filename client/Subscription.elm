module Subscription exposing (subs, send)

import Window exposing (resizes)
import WebSocket exposing (listen)

import Types exposing (..)

subs : Model -> Sub Msg
subs model =
    --let m = Debug.log "sub" model in
    Sub.batch
        [ resizes Window
        , listen model.server Listen
        ]

send : String -> String -> Cmd Msg
send = WebSocket.send
