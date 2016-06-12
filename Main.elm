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
import Json.Decode as Json exposing (..)

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
       , perform (\x -> None) Window Window.size
       )

type Msg = Window Size
         | None
         | MouseDown Mouse.Position
         | MouseMove Mouse.Position
         | MouseUp Mouse.Position
         | TouchMove Mouse.Position
         | TouchEnd

update : Msg -> Model -> (Model, Cmd Msg)

update message model =
    --let m = Debug.log "update-model" model in
    case (message, model) of
        (Window size, Desktop m) -> ( Desktop <| setSize size m, nop )
        (MouseDown pos, Desktop m) -> ( Desktop <| startMouseMovement pos m, nop )
        (MouseMove pos, Desktop m) -> ( Desktop <| addPositionToList pos m, nop )
        (MouseUp pos, Desktop m) -> ( Desktop <| endMouseMovement pos m, nop )
        (None, Desktop m) -> ( Desktop m, nop )
        (TouchMove pos, Desktop m) -> ( Phone <| touchMovement pos {shape = m.shape, size = m.size}, nop )
        (TouchMove pos, Phone m) -> ( Phone <| touchMovement pos m, nop )
        (TouchEnd, Phone m) -> ( Phone <| cancelTouch m, nop )
        (_, _) -> ( model, nop )

nop : Cmd Msg
nop = Cmd.none

setSize : Size -> CanvasModel-> CanvasModel
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
        w = toFloat model.size.width
        h = toFloat model.size.height
        x = toFloat pos.x
        y = toFloat pos.y

        {- conversion between mouse coord and canvas coord
         - mouse coord starts at top letf
         - canvas origin lies on center of the screen
        -}
        cx = x - w/2
        cy = -(y - h/2)

        newPath = (cx, cy) :: model.currentPath

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

touchMovement : Mouse.Position -> {shape: Shape, size: Size} -> {shape: Shape, size: Size}
touchMovement pos model =
    let
        --p = Debug.log "on:touch" pos
        --m = Debug.log "on:touch:model" model

        w = toFloat model.size.width
        h = toFloat model.size.height
        x = toFloat pos.x
        y = toFloat pos.y

        {- conversion between mouse coord and canvas coord
         - mouse coord starts at top letf
         - canvas origin lies on center of the screen
        -}
        cx = x - w/2
        cy = -(y - h/2)
        point = (cx, cy)
    in
        case model.shape of
            (p::ps)::xs -> { model | shape = (([point, p])++ps)::xs }
            ([])::xs -> { model | shape = ([point])::xs }
            [] -> { model | shape = [[point]] }

cancelTouch : {shape: Shape, size: Size} -> {shape: Shape, size: Size}
cancelTouch model =
    --Debug.log "cancel"
    {model | shape = [] :: model.shape}

subs : Model -> Sub Msg
subs model =
    Sub.batch
        [ downs MouseDown
        , moves MouseMove
        , ups MouseUp
        , resizes Window
        ]

{- event.changedTouches[0].clientX, event.changedTouches[0].clientY -}
touchDecoder : Decoder (Float, Float)
touchDecoder = Json.at ["changedTouches", "0"] <| Json.object2 (,) ("clientX" := float) ("clientY" := float)

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
        Desktop m -> canvas {shape = m.shape, size = m.size}
        Phone m -> canvas {shape = m.shape, size = m.size}

canvas : {shape: Shape, size: Size} -> Html Msg
canvas m =
    let
        --l = Debug.log "shape" m.shape
        --c = Debug.log "shape-counts" <| countShape m.shape
        paths = List.map path m.shape
        forms = List.map (traced defaultLine) paths

        w = m.size.width
        h = m.size.height
    in
        div [ onWithOptions
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
