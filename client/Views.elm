module Views exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Text exposing (..)
import Color exposing (rgb)
import FontAwesome exposing (server, eraser)

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
          [ div [onMouseOver OnHoverServer, onClick ShowServerModal] [server (rgb 0 0 0) 24, onHoverServer m]
          , div [onMouseOver OnHoverClear, onClick ClearAllDrawings] [eraser (rgb 0 0 0) 24, onHoverClear m]
          ]
      , showServerModal m
      , githubRibbon
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

showServerModal : Model -> Html Msg
showServerModal model =
  let
    active =
      if model.ext.showServerModal then "is-active"
      else ""
  in
  div [class <| "modal " ++ active]
    [ div [class "modal-background", onClick CloseServerModal][]
    , div [class "modal-card"]
        [ header [class "modal-card-head"]
            [ p [class "modal-card-title"] [Html.text "Enter <server-ip>:<port> to connect"] ]
        , section [class "modal-card-body"]
            [ p [class "control has-addons"]
                [ span [class "button is-success", disabled True] [Html.text "ws://"]
                , input [class "input", type' "text", placeholder "<server-ip>:<port>", onInput SaveTransientServer, on "keyup" <| enterKeyDecoder keyCode] []
                ]
            ]
        , footer [class "modal-card-foot"]
            [ a [class "button is-primary", onClick SaveServer] [Html.text "Connect Now"] ]
        ]
    ]

countShape : (List (List a)) -> List Int
countShape shape = List.map (\x -> List.length x) shape

githubRibbon : Html Msg
githubRibbon =
  a [href "https://github.com/rajasharan/elm-freehand-drawing"]
    [img
      [ Html.Attributes.style [("position", "absolute"), ("top", "0"), ("right", "0"), ("border", "0")]
      , src "https://camo.githubusercontent.com/a6677b08c955af8400f44c6298f40e7d19cc5b2d/68747470733a2f2f73332e616d617a6f6e6177732e636f6d2f6769746875622f726962626f6e732f666f726b6d655f72696768745f677261795f3664366436642e706e67"
      , alt "Fork me on Github"
      , attribute "data-canonical-src" "https://s3.amazonaws.com/github/ribbons/forkme_right_gray_6d6d6d.png"
      ] []
    ]
