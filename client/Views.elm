module Views exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Text exposing (..)
import Color exposing (rgb)
import FontAwesome exposing (server, eraser, pencil)

import Types exposing (..)
import Decoders exposing (..)

view : Model -> Html Msg
view model =
    drawCanvas model

drawCanvas : Model -> Html Msg
drawCanvas m =
    let
      --c = Debug.log "draw" m.clients
      --l = Debug.log "shape" m.shape
      --c = Debug.log "shape-counts" <| countShape m.shape
      paths = List.map path m.shape
      forms = List.map (traced defaultLine) paths

      clientShapeList = List.map (\c -> c.shape) m.clients
      clientPathList = List.map (\s -> List.map path s) clientShapeList
      clientFormsList = List.map (\p -> List.map (traced defaultLine) p) clientPathList
      flattenClientForm = List.concat clientFormsList

      w = m.size.width
      h = m.size.height
    in
      div []
        [ div
            [ class "canvas"
            , onWithOptions
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
            [ collage w h ( (banner m :: forms) ++ flattenClientForm )
              |> toHtml
            ]
        , div [class "icons", onMouseOut CancelHover]
            [ div [onMouseOver OnHoverServer] [server (rgb 0 0 0) 24, onHoverServer m]
            , div [onMouseOver OnHoverClear, onClick ClearAllDrawings] [eraser (rgb 0 0 0) 24, onHoverClear m]
            ]
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

onHoverServer : Model -> Html Msg
onHoverServer model =
  if model.ext.onHoverServer then
    span [class "tag"] [Html.text "Connect to Server"]
  else 
    span [][]

onHoverClear : Model -> Html Msg
onHoverClear model =
  if model.ext.onHoverClear then
    span [class "tag"] [Html.text "Clear Screen"]
  else
    span [][]

countShape : (List (List a)) -> List Int
countShape shape = List.map (\x -> List.length x) shape
