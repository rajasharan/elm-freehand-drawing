module Encoders exposing (..)

import Json.Encode as Json exposing (..)

import Types exposing (..)

encodeSocketMsg : SocketMsg -> String
encodeSocketMsg msg =
    case msg.kind of
        Initial -> encode 0 (object [("id", int msg.id), ("kind", string "initial"), ("x", null), ("y", null)])
        Point -> encode 0 (object [("id", int msg.id), ("kind", string "point"), ("x", float msg.x), ("y", float msg.y)])
        Cancel -> encode 0 (object [("id", int msg.id), ("kind", string "cancel"), ("x", null), ("y", null)])
