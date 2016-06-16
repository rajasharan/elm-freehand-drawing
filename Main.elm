import Html exposing (..)
import Html.App as App exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Text exposing (..)
import Window exposing (..)
import Task exposing (..)
import Mouse exposing (..)
import WebSocket exposing (..)
import Json.Decode as Json exposing (..)
import Random exposing (..)

main =
    App.program
        { init = init
        , update = update
        , subscriptions = subs
        , view = view
        }

type Model = Phone PhoneModel
           | Desktop CanvasModel

type alias CanvasModel =
    { shape : Shape
    , currentPath : Path
    , mouseMoving : Bool
    , size : Size
    }

type alias PhoneModel =
    { shape : Shape
    , size : Size
    }

type alias Shape = List Path
type alias Path = List (Float, Float)

init : (Model, Cmd Msg)
init = ( Desktop
             { shape = []
             , currentPath = []
             , mouseMoving = False
             , size = { width = 500, height = 500 }
             }
       , perform Error Window Window.size
       )

type Msg = Window Size
         | Error String
         | MouseDown Mouse.Position
         | MouseMove Mouse.Position
         | MouseUp Mouse.Position
         | TouchMove Mouse.Position
         | TouchStart
         | TouchEnd
         | Listen String

server : String
server = "ws://192.168.1.5:3000"

update : Msg -> Model -> (Model, Cmd Msg)
update message model =
    --let m = Debug.log "update-model" model in
    case (message, model) of
        (Window size, Desktop m)    -> ( Desktop <| setSize size m, nop )
        (MouseDown pos, Desktop m)  -> ( Desktop <| startMouseMovement pos m, nop )
        (MouseMove pos, Desktop m)  -> ( Desktop <| addPositionToList pos m, nop )
        (MouseUp pos, Desktop m)    -> ( Desktop <| endMouseMovement pos m, nop )
        (Error err, Desktop m)      -> ( Desktop m, print err )
        (TouchStart, Desktop m)     -> ( Phone <| resetPath {shape = m.shape, size = m.size}, nop )
        (TouchStart, Phone m)       -> ( Phone <| resetPath m, nop )
        (TouchMove pos, Desktop m)  -> ( Phone <| touchMovement pos {shape = m.shape, size = m.size}, nop )
        (TouchMove pos, Phone m)    -> ( Phone <| touchMovement pos m, sendPosition pos m.size )
        (TouchEnd, Phone m)         -> ( Phone <| resetPath m, sendCancel )
        --(Listen str, Desktop m)     -> ( decodeAndAddShapeDesktop str m, nop )
        (Listen str, Phone m)       -> ( decodeAndAddShapePhone str m, nop )
        (_, _)                      -> ( model, nop )

nop : Cmd Msg
nop = Cmd.none

print : String -> Cmd Msg
print str =
    let s = Debug.log "" str
    in nop

setSize : Size -> CanvasModel -> CanvasModel
setSize size model = { model | size = size }

startMouseMovement : Mouse.Position -> CanvasModel -> CanvasModel
startMouseMovement pos model =
    --let p = Debug.log "sub:start" pos in
    { model | mouseMoving = True
            , currentPath = [] 
    }

addPositionToList : Mouse.Position -> CanvasModel -> CanvasModel
addPositionToList pos model =
    let 
        --p = Debug.log "sub:move" pos
        newPos = convertMouseToCanvasCoord pos model.size
        newPath = newPos :: model.currentPath

        replace path shape =
            case (List.head shape) of
                Nothing -> path :: shape
                Just _ -> path :: List.drop 1 shape
    in
    if model.mouseMoving then
        { model | currentPath = newPath
                , shape = replace newPath model.shape
        }
    else
        model

endMouseMovement : Mouse.Position -> CanvasModel -> CanvasModel
endMouseMovement pos model =
    --let p = Debug.log "sub:end" pos in
    --Debug.log "end"
    { model | mouseMoving = False
            , shape = model.currentPath :: model.shape
    }

resetPath : {shape: Shape, size: Size} -> {shape: Shape, size: Size}
resetPath model =
    Debug.log "touch:start"
    { model | shape = [] :: model.shape }

touchMovement : Mouse.Position -> {shape: Shape, size: Size} -> {shape: Shape, size: Size}
touchMovement pos model =
    let
        --p = Debug.log "on:touch" pos
        --m = Debug.log "on:touch:model" model
        point = convertMouseToCanvasCoord pos model.size
    in
        addPointToShape point model

--cancelTouch : {shape: Shape, size: Size} -> {shape: Shape, size: Size}
--cancelTouch model =
    ----Debug.log "cancel"
    --{model | shape = [] :: model.shape}

addPointToShape : (Float, Float) -> {shape: Shape, size: Size} -> {shape: Shape, size: Size}
addPointToShape point model =
    case model.shape of
        (p::ps)::xs  -> { model | shape = (([point, p])++ps)::xs }
        ([])::xs     -> { model | shape = ([point])::xs }
        []           -> { model | shape = [[point]] }

sendPosition : Mouse.Position -> Size -> Cmd Msg
sendPosition pos size =
    let point = convertMouseToCanvasCoord pos size
    in send server <| toString point

sendCancel : Cmd Msg
sendCancel = send server "Cancel"

{-
decodeAndAddShapeDesktop : String -> CanvasModel -> Model
decodeAndAddShapeDesktop str m =
    let
        point = decodePoint str
    in
        case point of
            Just p -> Desktop (addPointToShape p {shape = m.shape, size = m.size})
            Nothing -> Desktop m
-}

decodeAndAddShapePhone : String -> PhoneModel -> Model
decodeAndAddShapePhone str m =
    let
        point = decodePoint str
    in
        case point of
            Just p -> Phone (addPointToShape p {shape = m.shape, size = m.size})
            Nothing -> Phone (resetPath m)

decodePoint : String -> Maybe (Float, Float)
decodePoint point =
    let
        result = decodeString (tuple2 (,) Json.float Json.float) point
    in
        case result of
            Ok r -> Just r
            Err e -> Nothing

{- conversion between mouse coord and canvas coord
- mouse coord starts at top left
- canvas origin lies on center of the screen
-}
convertMouseToCanvasCoord : Mouse.Position -> Size -> (Float, Float)
convertMouseToCanvasCoord pos size =
    let
        w = toFloat size.width
        h = toFloat size.height
        x = toFloat pos.x
        y = toFloat pos.y

        x' = x - w/2
        y' = -(y - h/2)
    in
        (x', y')

subs : Model -> Sub Msg
subs model =
    Sub.batch
        [ downs MouseDown
        , moves MouseMove
        , ups MouseUp
        , resizes Window
        , listen server Listen
        ]

{- event.changedTouches[0].clientX, event.changedTouches[0].clientY -}
touchDecoder : Decoder (Float, Float)
touchDecoder = Json.at ["changedTouches", "0"] <| Json.object2 (,) ("clientX" := Json.float) ("clientY" := Json.float)

{- Mouse.Position is aliased as {x: Int, y: Int} -}
touchPosDecoder : Decoder Mouse.Position
touchPosDecoder =
    let
        converter tuple =
            case tuple of
                (x, y) -> {x = round x, y = round y}
    in
        Json.map converter touchDecoder

view : Model -> Html Msg
view model =
    case model of
        Desktop m -> drawCanvas {shape = m.shape, size = m.size}
        Phone m -> drawCanvas {shape = m.shape, size = m.size}

drawCanvas : {shape: Shape, size: Size} -> Html Msg
drawCanvas m =
    let
        --l = Debug.log "shape" m.shape
        --c = Debug.log "shape-counts" <| countShape m.shape
        paths = List.map path m.shape
        forms = List.map (traced defaultLine) paths

        w = m.size.width
        h = m.size.height
    in
        div [ onWithOptions
                "touchstart"
                { stopPropagation = True, preventDefault = True }
                (Json.succeed TouchStart)
            , onWithOptions
                "touchmove"
                { stopPropagation = True, preventDefault = True }
                (Json.map TouchMove touchPosDecoder)
            , on "touchend" (Json.succeed TouchEnd)
            ]
            [ collage w h (banner m :: forms)
              |> toHtml
            ]

banner : {shape: Shape, size: Size} -> Form
banner model =
    fromString "Freehand Drawing"
    |> bold
    |> monospace
    |> Text.height 40
    |> line Under
    |> justified
    |> container model.size.width model.size.height (midTopAt (relative 0.5) (relative 0.01))
    |> toForm

countShape : (List (List a)) -> List Int
countShape shape = List.map (\x -> List.length x) shape
