module Update exposing (update)

import Window
import Mouse

import Types exposing (..)
import Utils exposing (..)
import Subscription exposing (send)
import Decoders exposing (decodePoint, decodeSocketMsg)
import Encoders exposing (encodeSocketMsg)
import Clients exposing (drawClient)

update : Msg -> Model -> (Model, Cmd Msg)
update message model =
    --let m = Debug.log "update-model" model in
    case message of
        Window size -> ( setSize size model, nop )
        Error err -> ( model, print err )
        TouchStart -> ( start model, nop )
        TouchMove pos -> ( draw pos model, sendPosition pos model )
        TouchEnd -> ( stop model, sendCancel model )
        Listen str -> ( drawClient (decodeSocketMsg str) model, nop )

start : Model -> Model
start model =
    --Debug.log "touch:start"
    { model | shape = [] :: model.shape
            , moving = True
    }

stop : Model -> Model
stop model =
    { model | shape = [] :: model.shape
            , moving = False
    }

draw : Mouse.Position -> Model -> Model
draw pos model =
    let
        --p = Debug.log "on:touch" pos
        --m = Debug.log "on:touch:model" model
        point = convertMouseToCanvasCoord pos model.size
    in
        if model.moving then
            addPointToModel point model
        else
            model

addPointToModel : (Float, Float) -> Model -> Model
addPointToModel point model =
    { model | shape = modifyShape point model.shape }

sendPosition : Mouse.Position -> Model -> Cmd Msg
sendPosition pos m =
    let 
        normalizePoint' = normalizePoint m.size
        (x, y) = normalizePoint' <| convertMouseToCanvasCoord pos m.size
    in
        if m.moving then
            send m.server <| encodeSocketMsg {id = m.id, kind = Point, x = x, y = y }
        else
            nop

sendCancel : Model -> Cmd Msg
sendCancel m = send m.server <| encodeSocketMsg {id = m.id, kind = Cancel, x = 0, y = 0}
