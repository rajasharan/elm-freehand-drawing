module Decoders exposing (..)

import Mouse
import Json.Decode as Json exposing (..)

import Types exposing (..)

{- event.changedTouches[0].clientX, event.changedTouches[0].clientY -}
touchDecoder : Decoder (Float, Float)
touchDecoder = at ["changedTouches", "0"] mouseDecoder

{- even.clientX, event.clientY -}
mouseDecoder : Decoder (Float, Float)
mouseDecoder = object2 (,) ("clientX" := float) ("clientY" := float)

{- Mouse.Position is aliased as {x: Int, y: Int} -}
positionDecoder : MouseType -> Decoder Mouse.Position
positionDecoder mouseType =
    let
        converter tuple =
            case tuple of
                (x, y) -> {x = round x, y = round y}
    in
        case mouseType of
            Mouse -> Json.map converter mouseDecoder
            Touch -> Json.map converter touchDecoder

touchStartDecoder : Decoder Msg
touchStartDecoder = succeed TouchStart

touchMoveDecoder : Decoder Msg
touchMoveDecoder = Json.map TouchMove (positionDecoder Touch)

touchEndDecoder : Decoder Msg
touchEndDecoder = succeed TouchEnd

mouseStartDecoder : Decoder Msg
mouseStartDecoder = touchStartDecoder

mouseMoveDecoder : Decoder Msg
mouseMoveDecoder = Json.map TouchMove (positionDecoder Mouse)

mouseEndDecoder : Decoder Msg
mouseEndDecoder = touchEndDecoder

enterKeyDecoder : Decoder Int -> Decoder Msg
enterKeyDecoder keyCode = Json.map EnterKey keyCode

decodePoint : String -> Maybe (Float, Float)
decodePoint point =
    let
        result = decodeString (tuple2 (,) float float) point
    in
        case result of
            Ok r -> Just r
            Err e -> Nothing

decodeSocketMsg : String -> Result String SocketMsg
decodeSocketMsg message =
    flip decodeString message
    <| oneOf [ object4 SocketMsg ("id" := int) ("kind" := initial) (succeed 0) (succeed 0)
             , object4 SocketMsg ("id" := int) ("kind" := point) ("x" := float) ("y" := float)
             , object4 SocketMsg ("id" := int) ("kind" := cancel) (succeed 0) (succeed 0)
             , object4 SocketMsg ("id" := int) ("kind" := clearAll) (succeed 0) (succeed 0)
             ]

initial : Decoder SocketKind
initial = string `andThen`
              (\s -> case s of
                       "initial" -> succeed Initial
                       _ -> fail <| "Server sent wrong kind: " ++ s
              )

point : Decoder SocketKind
point = string `andThen`
            (\s -> case s of
                     "point" -> succeed Point
                     _ -> fail <| "Server sent wrong kind: " ++ s
            )

cancel : Decoder SocketKind
cancel = string `andThen`
             (\s -> case s of
                      "cancel" -> succeed Cancel
                      _ -> fail <| "Server sent wrong kind: " ++ s
             )
clearAll : Decoder SocketKind
clearAll = string `andThen`
             (\s -> case s of
                      "clearAll" -> succeed ClearAll
                      _ -> fail <| "Server sent wrong kind: " ++ s
             )
