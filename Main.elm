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

type alias Model =
    { shape : Shape
    , size : Size
    , moving: Bool
    , server: String
    }

type alias Shape = List Path
type alias Path = List (Float, Float)

type MouseType = Mouse | Touch

init : (Model, Cmd Msg)
init = ( { shape = []
         , size = { width = 500, height = 500 }
         , moving = False
         , server = "ws://192.168.1.5:3000"
         }
       , perform Error Window Window.size
       )

type Msg = Window Size
         | Error String
         | TouchMove Mouse.Position
         | TouchStart
         | TouchEnd
         | Listen String

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

nop : Cmd Msg
nop = Cmd.none

print : String -> Cmd Msg
print str =
    let s = Debug.log "" str
    in nop

setSize : Size -> Model -> Model
setSize size model = { model | size = size }

start : Model -> Model
start model =
    --Debug.log "touch:start"
    { model | shape = [] :: model.shape
            , moving = True
    }

stop : Model -> Model
stop model = { model | moving = False }

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
    in send m.server <| toString point

sendCancel : Model -> Cmd Msg
sendCancel m = send m.server "Cancel"

normalizePoint : Size -> (Float, Float) -> (Float, Float)
normalizePoint size point =
    let
        w = (toFloat size.width)/2
        h = (toFloat size.height)/2
        (x,y) = point
        x' = x/w
        y' = y/h
    in
        (x', y')

denormalizePoint : Size -> (Float, Float) -> (Float, Float)
denormalizePoint size point =
    let
        w = (toFloat size.width)/2
        h = (toFloat size.height)/2
        (x,y) = point
        x' = w*x
        y' = h*y
    in
        (x', y')

decodeAndAddShape : String -> Model -> Model
decodeAndAddShape str m =
    let
        denormalizePoint' = denormalizePoint m.size
        point = decodePoint str
    in
        case point of
            Just p -> addPointToShape (denormalizePoint' p) m
            Nothing -> stop m

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
        [ resizes Window
        , listen model.server Listen
        ]

{- event.changedTouches[0].clientX, event.changedTouches[0].clientY -}
touchDecoder : Decoder (Float, Float)
touchDecoder = Json.at ["changedTouches", "0"] mouseDecoder

{- even.clientX, event.clientY -}
mouseDecoder : Decoder (Float, Float)
mouseDecoder = Json.object2 (,) ("clientX" := Json.float) ("clientY" := Json.float)

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

touchMoveDecoder : Decoder Msg
touchMoveDecoder = Json.map TouchMove (positionDecoder Touch)

mouseMoveDecoder : Decoder Msg
mouseMoveDecoder = Json.map TouchMove (positionDecoder Mouse)

view : Model -> Html Msg
view model =
    drawCanvas model

drawCanvas : Model -> Html Msg
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
                touchMoveDecoder
            , on "touchend" (Json.succeed TouchEnd)
            , on "mousedown" (Json.succeed TouchStart)
            , on "mousemove" mouseMoveDecoder
            , on "mouseup" (Json.succeed TouchEnd)
            ]
            [ collage w h (banner m :: forms)
              |> toHtml
            ]

banner : Model -> Form
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
