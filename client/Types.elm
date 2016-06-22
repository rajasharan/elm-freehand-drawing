module Types exposing (..)

import Window exposing (Size)
import Mouse

type alias Model =
    { id : Int
    , shape : Shape
    , size : Size
    , moving : Bool
    , server : String
    , clients : List Client
    , ext : ExternalProps
    }

type alias Client =
    { id : Int
    , shape : Shape
    }

type alias ExternalProps =
    { onHoverServer : Bool
    , onHoverClear : Bool
    , showServerModal : Bool
    , transientServer : String
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
         | OnHoverServer
         | OnHoverClear
         | CancelHover
         | ClearAllDrawings
         | ShowServerModal
         | SaveTransientServer String
         | SaveServer
         | CloseServerModal
         | EnterKey Int

type alias SocketMsg =
    { id : Id
    , kind : SocketKind
    , x : Float
    , y : Float
    }

type alias Id = Int
type SocketKind = Initial | Point | Cancel | ClearAll
