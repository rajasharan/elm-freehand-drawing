module Types exposing (..)

import Window exposing (Size)
import Mouse

type alias Model =
    { shape : Shape
    , size : Size
    , moving: Bool
    , server: String
    }

type alias Shape = List Path
type alias Path = List (Float, Float)

type MouseType = Mouse | Touch

type Msg = Window Size
         | Error String
         | TouchMove Mouse.Position
         | TouchStart
         | TouchEnd
         | Listen String

