module MiniWorldWar.Server.Host exposing
    ( HostMsg(..)
    , exit
    , submit
    , waitingForClient
    )

import Dict exposing (Dict)
import Http exposing (Error(..), Expect)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
import MiniWorldWar.Board as Board exposing (MoveBoard, Unit, UnitBoard)
import MiniWorldWar.Game as Game exposing (Game)
import MiniWorldWar.Server as Server
    exposing
        ( Response(..)
        , RunningGameTable
        , httpDelete
        , httpPut
        , openGameRoute
        , runningGameRoute
        )
import Task
import Time exposing (Posix)


type HostMsg
    = WaitingForClient
    | UpdateMoveBoard MoveBoard
    | Submit


type alias HostResponse =
    Response HostMsg


exit : String -> Cmd (Response Never)
exit id =
    let
        response : Result Error () -> Response Never
        response result =
            Reset
    in
    httpDelete
        { url = runningGameRoute ++ "/" ++ id
        , expect = Http.expectWhatever response
        }



{------------------------
   RunningGame
------------------------}


waitingForClient : String -> Int -> Cmd HostResponse
waitingForClient id oldTime =
    let
        response : Result Error Game -> HostResponse
        response result =
            case result of
                Ok ({ lastUpdated, moveBoard } as game) ->
                    if lastUpdated > oldTime then
                        Please <| UpdateMoveBoard moveBoard

                    else
                        Idle

                Err error ->
                    case error of
                        BadBody _ ->
                            (error |> Debug.log "badBodyError")
                                |> always Exit

                        _ ->
                            (error |> Debug.log "error")
                                |> always DropRunningGameTable
    in
    Http.get
        { url = runningGameRoute ++ "/" ++ id
        , expect = Http.expectJson response (D.field "result" <| Game.decoder)
        }

submit : String -> Game -> Cmd HostResponse
submit id game =
    let
        value : Value
        value =
            game |> Game.encode

        response : Result Error () -> HostResponse
        response result =
            case result of
                Ok () ->
                    Idle

                Err error ->
                    case error of
                        BadBody _ ->
                            (error |> Debug.log "badBodyError")
                                |> always DropRunningGameTable

                        _ ->
                            (error |> Debug.log "error")
                                |> always Exit
    in
    httpPut
        { url = runningGameRoute ++ "/" ++ id
        , expect = Http.expectWhatever response
        , body = Http.jsonBody value
        }
