module Views exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Text exposing (..)

import Types exposing (..)
import Decoders exposing (..)

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
                touchStartDecoder
            , onWithOptions
                "touchmove"
                { stopPropagation = True, preventDefault = True }
                touchMoveDecoder
            , on "touchend" touchEndDecoder
            , on "mousedown" mouseStartDecoder
            , on "mousemove" mouseMoveDecoder
            , on "mouseup" mouseEndDecoder
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
