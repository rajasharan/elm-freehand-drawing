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

decodePoint : String -> Maybe (Float, Float)
decodePoint point =
    let
        result = decodeString (tuple2 (,) float float) point
    in
        case result of
            Ok r -> Just r
            Err e -> Nothing
