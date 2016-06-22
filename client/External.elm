module External exposing (..)

import List

import Types exposing (..)

onHoverServer : Model -> Model
onHoverServer model =
    { model | ext = { onHoverServer = True, onHoverClear = False } }

onHoverClear : Model -> Model
onHoverClear model =
    { model | ext = { onHoverServer = False, onHoverClear = True } }

cancelHover : Model -> Model
cancelHover model =
    { model | ext = { onHoverServer = False, onHoverClear = False } }

clearAll : Model -> Model
clearAll model =
    { model | shape = []
            , clients = clearAllClients model.clients
    }

clearAllClients : List Client -> List Client
clearAllClients clients =
    List.map (\c -> {c | shape = []}) clients
