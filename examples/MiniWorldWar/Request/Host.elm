module MiniWorldWar.Request.Host exposing
    ( HostMsg(..)
    , exit
    , submit
    , waitingForClient
    )

import Http exposing (Error(..))
import Json.Decode as D
import Json.Encode exposing (Value)
import MiniWorldWar.Data.Board exposing (MoveBoard)
import MiniWorldWar.Data.Game as Game exposing (Game)
import MiniWorldWar.Request exposing (Response(..), httpDelete, httpPut, runningGameRoute)


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
            case result of
                Ok () ->
                    Reset

                Err error ->
                    error |> ResetWithError
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
                Ok { lastUpdated, moveBoard } ->
                    if lastUpdated > oldTime then
                        Please <| UpdateMoveBoard moveBoard

                    else
                        Idle

                Err error ->
                    case error of
                        BadBody _ ->
                            error |> ExitWithError

                        _ ->
                            error |> DropRunningGameTable
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
                            error |> DropRunningGameTable

                        _ ->
                            error |> ExitWithError
    in
    httpPut
        { url = runningGameRoute ++ "/" ++ id
        , expect = Http.expectWhatever response
        , body = Http.jsonBody value
        }
