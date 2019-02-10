module MiniWorldWar.Server.Host exposing (
    HostResponse(..), newRound,endGame
            , updateGameTable
        , dropGameTable
    )

import Http exposing (Error(..), Expect)
import Json.Encode as E exposing (Value)
import MiniWorldWar.Game as Game exposing (Game)
import MiniWorldWar.Server as Server exposing( RunningGameTable
        , httpDelete
        , httpPut
        , openGameRoute
        , runningGameRoute

        )
import Time exposing (Posix)
import Dict exposing (Dict)


type HostResponse
    = NewRound
    | EndGame
    | UpdateGameTable RunningGameTable
    | DropGameTable
    | StopHosting
    | UpdateAsHost Posix
    | IdleAsHost



{------------------------
   RunningGame
------------------------}
updateGameTable : RunningGameTable -> Cmd HostResponse
updateGameTable table =
    let
        value : Value
        value =
            table |> E.dict identity Game.encode

        response : Result Error () -> HostResponse
        response result =
            case result of
                Ok () ->
                    IdleAsHost

                Err error ->
                    EndGame
    in
    httpPut
        { url = runningGameRoute
        , expect = Http.expectWhatever response
        , body = Http.jsonBody value
        }

dropGameTable : Cmd HostResponse
dropGameTable =
    let
        response : Result Error () -> HostResponse
        response result =
            StopHosting
    in
    httpDelete
        { url = runningGameRoute
        , expect = Http.expectWhatever response
        }

endGame : String -> Posix -> Cmd HostResponse
endGame currentId now =
    let
        oneHour : Int
        oneHour =
            1000 * 60 * 60

        response : Result Error RunningGameTable -> HostResponse
        response result =
            case result of
                Ok dict ->
                    UpdateGameTable (dict
                        |> Dict.filter
                            (\id { lastUpdated} ->
                                id /= currentId
                                && lastUpdated + oneHour <= (now |> Time.posixToMillis)
                            ))

                Err error ->
                    case error of
                        BadBody _ ->
                            (error |> Debug.log "badBodyError:")
                                |> always DropGameTable

                        _ ->
                            (error |> Debug.log "error:")
                                |> always StopHosting
    in
    Http.get
        { url = runningGameRoute
        , expect = Http.expectJson response Server.runningGameTableDecoder
        }


newRound : String -> Game -> Cmd HostResponse
newRound id game =
    let
        value : Value
        value =
            game |> Game.encode

        response : Result Error () -> HostResponse
        response result =
            case result of
                Ok () ->
                    IdleAsHost

                Err error ->
                    
                    case error of
                        BadBody _ ->
                            (error |> Debug.log "badBodyError:")
                                |> always DropGameTable
                        _ -> 
                            (error |> Debug.log "error:")
                                |> always EndGame
    in
    httpPut
        { url = runningGameRoute ++ "/" ++ id
        , expect = Http.expectWhatever response
        , body = Http.jsonBody value
        }
