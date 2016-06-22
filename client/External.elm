module External exposing (..)

import List

import Types exposing (..)

onHoverServer : Model -> Model
onHoverServer model =
    let onHover ext = { ext | onHoverServer = True }
    in { model | ext = onHover model.ext }

onHoverClear : Model -> Model
onHoverClear model =
    let onHover ext = { ext | onHoverClear = True }
    in { model | ext = onHover model.ext }

cancelHover : Model -> Model
cancelHover model =
    let cancel ext = { ext | onHoverServer = False, onHoverClear = False }
    in { model | ext = cancel model.ext }

clearAll : Model -> Model
clearAll model =
    { model | shape = []
            , clients = clearAllClients model.clients
    }

clearAllClients : List Client -> List Client
clearAllClients clients =
    List.map (\c -> {c | shape = []}) clients

showServerModal : Model -> Model
showServerModal model =
    let showServer ext = { ext | showServerModal = True, transientServer = "" }
    in { model | ext = showServer model.ext } --|> Debug.log "show"

saveTransientServer : String -> Model -> Model
saveTransientServer server model =
    let saveTransient server ext = { ext | transientServer = server }
    in { model | ext = saveTransient server model.ext }

saveServer : Model -> Model
saveServer model =
    let close ext = { ext | showServerModal = False }
    in { model | server = "ws://" ++ model.ext.transientServer
               , ext = close model.ext
       }

closeServerModal : Model -> Model
closeServerModal model =
    let closeServer ext = { ext | showServerModal = False }
    in { model | ext = closeServer model.ext }

enterKeyPressed : Int -> Model -> Model
enterKeyPressed keyCode model =
    if keyCode == 13 then
        saveServer model
    else
        model
