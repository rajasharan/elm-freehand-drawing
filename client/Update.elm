module Update exposing (update)

import Window
import Mouse

import Types exposing (..)
import Utils exposing (..)
import Subscription exposing (send)
import Decoders exposing (decodePoint)

update : Msg -> Model -> (Model, Cmd Msg)
update message model =
    --let m = Debug.log "update-model" model in
    case message of
        Window size -> ( setSize size model, nop )
        Error err -> ( model, print err )
        TouchStart -> ( start model, nop )
        TouchMove pos -> ( draw pos model, sendPosition pos model )
        TouchEnd -> ( stop model, sendCancel model )
        Listen str -> ( decodeAndAddShape str model, nop )

start : Model -> Model
start model =
    --Debug.log "touch:start"
    { model | shape = [] :: model.shape
            , moving = True
    }

stop : Model -> Model
stop model = { model | shape = [] :: model.shape
                     , moving = False }

draw : Mouse.Position -> Model -> Model
draw pos model =
    let
        --p = Debug.log "on:touch" pos
        --m = Debug.log "on:touch:model" model
        point = convertMouseToCanvasCoord pos model.size
    in
        if model.moving then
            addPointToShape point model
        else
            model

addPointToShape : (Float, Float) -> Model -> Model
addPointToShape point model =
    case model.shape of
        (p::ps)::xs  -> { model | shape = (([point, p])++ps)::xs }
        ([])::xs     -> { model | shape = ([point])::xs }
        []           -> { model | shape = [[point]] }

sendPosition : Mouse.Position -> Model -> Cmd Msg
sendPosition pos m =
    let 
        normalizePoint' = normalizePoint m.size
        point = normalizePoint' <| convertMouseToCanvasCoord pos m.size
    in
        if m.moving then
            send m.server <| toString point
        else
            nop

sendCancel : Model -> Cmd Msg
sendCancel m = send m.server "Cancel"

decodeAndAddShape : String -> Model -> Model
decodeAndAddShape str m =
    let
        denormalizePoint' = denormalizePoint m.size
        point = decodePoint str
    in
        case point of
            Just p -> addPointToShape (denormalizePoint' p) m
            Nothing -> stop m
