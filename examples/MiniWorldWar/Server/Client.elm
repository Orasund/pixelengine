module MiniWorldWar.Server.Client exposing
    ( ClientMsg(..)
    , endGame
    , exit
    , joinGame
    , submitMoveBoard
    , updateGameTable
    , waitingForHost
    )

import Dict
import Http exposing (Error(..), Expect)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
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
import Time exposing (Posix)


type ClientMsg
    = UpdateGameTable RunningGameTable
    | WaitingForHost
    | UpdateGame Game
    | Ready
    | EndGame


type alias ClientResponse =
    Response ClientMsg


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


submitMoveBoard : Game -> String -> Cmd ClientResponse
submitMoveBoard game id =
    let
        value : Value
        value =
            game |> Game.encode

        response : Result Error () -> ClientResponse
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
                                |> always (Please EndGame)
    in
    httpPut
        { url = runningGameRoute ++ "/" ++ id
        , expect = Http.expectWhatever response
        , body = Http.jsonBody value
        }


waitingForHost : String -> Int -> Cmd ClientResponse
waitingForHost id oldTime =
    let
        response : Result Error Game -> ClientResponse
        response result =
            case result of
                Ok ({ lastUpdated, moveBoard } as game) ->
                    if lastUpdated > oldTime then
                        Please <| UpdateGame game

                    else
                        Idle

                Err error ->
                    case error of
                        BadBody _ ->
                            (error |> Debug.log "badBodyError")
                                |> always (Please EndGame)

                        _ ->
                            (error |> Debug.log "error")
                                |> always DropRunningGameTable
    in
    Http.get
        { url = runningGameRoute ++ "/" ++ id
        , expect = Http.expectJson response (D.field "result" <| Game.decoder)
        }


joinGame : String -> Game -> Cmd ClientResponse
joinGame id game =
    let
        value : Value
        value =
            Game.encode game

        response : Result Error () -> ClientResponse
        response result =
            case result of
                Ok () ->
                    (id |> Debug.log "id")
                        |> always (Please WaitingForHost)

                Err _ ->
                    Reset
    in
    Http.post
        { url = runningGameRoute ++ "/" ++ id
        , body = Http.jsonBody value
        , expect = Http.expectWhatever response
        }


updateGameTable : RunningGameTable -> Cmd ClientResponse
updateGameTable table =
    let
        value : Value
        value =
            table |> E.dict identity Game.encode

        response : Result Error () -> ClientResponse
        response result =
            case result of
                Ok () ->
                    Idle

                Err error ->
                    DropRunningGameTable
    in
    httpPut
        { url = runningGameRoute
        , expect = Http.expectWhatever response
        , body = Http.jsonBody value
        }


endGame : String -> Posix -> Cmd ClientResponse
endGame currentId now =
    let
        oneHour : Int
        oneHour =
            1000 * 60 * 60

        response : Result Error RunningGameTable -> ClientResponse
        response result =
            case result of
                Ok dict ->
                    Please <|
                        UpdateGameTable
                            (dict
                                |> Dict.filter
                                    (\id { lastUpdated } ->
                                        (id /= currentId)
                                            && (lastUpdated + oneHour <= (now |> Time.posixToMillis))
                                    )
                            )

                Err error ->
                    case error of
                        BadBody _ ->
                            (error |> Debug.log "badBodyError")
                                |> always DropRunningGameTable

                        _ ->
                            (error |> Debug.log "error")
                                |> always Reset
    in
    Http.get
        { url = runningGameRoute
        , expect = Http.expectJson response Server.runningGameTableDecoder
        }
