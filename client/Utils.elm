module Utils exposing (..)

import Window
import Mouse

import Types exposing (..)

setSize : Window.Size -> Model -> Model
setSize size model = { model | size = size }

normalizePoint : Window.Size -> (Float, Float) -> (Float, Float)
normalizePoint size point =
    let
        w = (toFloat size.width)/2
        h = (toFloat size.height)/2
        (x,y) = point
        x' = x/w
        y' = y/h
    in
        (x', y')

denormalizePoint : Window.Size -> (Float, Float) -> (Float, Float)
denormalizePoint size point =
    let
        w = (toFloat size.width)/2
        h = (toFloat size.height)/2
        (x,y) = point
        x' = w*x
        y' = h*y
    in
        (x', y')

{- conversion between mouse coord and canvas coord
- mouse coord starts at top left
- canvas origin lies on center of the screen
-}
convertMouseToCanvasCoord : Mouse.Position -> Window.Size -> (Float, Float)
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

nop : Cmd Msg
nop = Cmd.none

print : String -> Cmd Msg
print str =
    let s = Debug.log "" str
    in nop
