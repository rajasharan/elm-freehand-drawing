module Clients exposing (drawClient)

import List.Extra exposing (..)

import Types exposing (..)
import Utils exposing (..)

drawClient : Result String SocketMsg -> Model -> Model
drawClient result model =
    case result of
        Err e ->
            let p = Debug.log "decode" e
            in model
        Ok s ->
            --let p = Debug.log "decode" s in
            drawClient' s model

drawClient' : SocketMsg -> Model -> Model
drawClient' socket model =
    case socket.kind of
        Initial -> { model | clients = resetClient socket.id model.clients, id = socket.id }
        Cancel -> { model | clients = resetClient socket.id model.clients }
        Point -> addPointToClient socket.id socket.x socket.y model

resetClient : Id -> List Client -> List Client
resetClient id clients =
    let
        c' = find (\c -> c.id == id) clients 
    in
        case c' of
            Nothing -> { id = id, shape = [] } :: clients
            Just client ->
                replaceIf
                    (\c -> c.id == id)
                    { id = id, shape = [] :: client.shape }
                    clients

addPointToClient : Id -> Float -> Float -> Model -> Model
addPointToClient id x y model =
    let
        point = denormalizePoint model.size (x, y)
    in
        { model | clients = modifyClient id point model.clients }

modifyClient : Id -> (Float, Float) -> List Client -> List Client
modifyClient id point clients =
    let
        --i' = Debug.log "modifyClient:id" id
        c' = find (\c -> c.id == id) clients
    in
        case c' of
            Nothing -> {id = id, shape = []}::clients
            Just client ->
                replaceIf
                    (\c -> c.id == id)
                    (replacePointInClient point client)
                    clients

replacePointInClient : (Float, Float) -> Client -> Client
replacePointInClient point client =
    { client | shape = modifyShape point client.shape }
