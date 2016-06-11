import Html exposing (..)
import Html.App as App exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Window exposing (..)
import Task exposing (..)
import Mouse exposing (..)

main =
    App.program
        { init = init
        , update = update
        , subscriptions = subs
        , view = view
        }

type alias Model =
    { shape : Shape
    , currentPath : Path
    , mouseCurPos : Mouse.Position
    , mouseMoving : Bool
    , size : Size
    }

type alias Shape = List Path
type alias Path = List (Float, Float)

init : (Model, Cmd Msg)
init = ( { shape = []
         , currentPath = []
         , mouseCurPos = { x = 0, y = 0 }
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

update : Msg -> Model -> (Model, Cmd Msg)

update message model =
    --let m = Debug.log "update-model" model in
    case message of
        Window size -> ( setSize size model, nop )
        MouseDown pos -> ( startMouseMovement pos model, nop )
        MouseMove pos -> ( addPositionToList pos model, nop )
        MouseUp pos -> ( endMouseMovement pos model, nop )
        None -> ( model, nop )

nop : Cmd Msg
nop = Cmd.map (\x -> None) Cmd.none

setSize : Size -> Model -> Model
setSize size model = { model | size = size }

startMouseMovement : Mouse.Position -> Model -> Model
startMouseMovement pos model = { model | mouseMoving = True, mouseCurPos = pos }

addPositionToList : Mouse.Position -> Model -> Model
addPositionToList pos model =
    let 
        w = model.size.width
        h = model.size.height
        x = toFloat pos.x
        y = toFloat pos.y
        cx = x - (toFloat w)/2
        cy = -(y - (toFloat h)/2)
        replace path shape =
            case (List.head shape) of
                Nothing -> path :: shape
                Just _ -> path :: List.drop 1 shape
    in
    if model.mouseMoving then
        { model | currentPath = (cx, cy) :: model.currentPath
                , shape = replace model.currentPath model.shape
        }
    else
        model

endMouseMovement : Mouse.Position -> Model -> Model
endMouseMovement pos model = { model | mouseMoving = False
                                     --, shape = model.currentPath :: model.shape
                                     , currentPath = []
                             }

subs : Model -> Sub Msg
subs model =
        Sub.batch
            [ downs MouseDown
            , moves MouseMove
            , ups MouseUp
            , resizes Window
            ]

view : Model -> Html Msg
view model =
    let
        l = Debug.log "shape" model.shape
        paths = List.map path model.shape
        forms = List.map (traced defaultLine) paths

        getWidth {width} = width
        getHeight {height} = height

        s = model.size
        w = getWidth s
        h = getHeight s
    in
        collage w h forms
        |> toHtml

