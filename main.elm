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
    , mouseMoving : Bool
    , size : Size
    }

type alias Shape = List Path
type alias Path = List (Float, Float)

init : (Model, Cmd Msg)
init = ( { shape = []
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
startMouseMovement pos model =
    { model | mouseMoving = True
    , currentPath = [] 
    }

addPositionToList : Mouse.Position -> Model -> Model
addPositionToList pos model =
    let 
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

endMouseMovement : Mouse.Position -> Model -> Model
endMouseMovement pos model =
    { model | mouseMoving = False
    , shape = model.currentPath :: model.shape
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
        --l = Debug.log "shape" model.shape
        --c = Debug.log "shape-counts" <| countShape model.shape
        paths = List.map path model.shape
        forms = List.map (traced defaultLine) paths

        w = model.size.width
        h = model.size.height
    in
        collage w h forms
        |> toHtml

countShape : (List (List a)) -> List Int
countShape shape = List.map (\x -> List.length x) shape
