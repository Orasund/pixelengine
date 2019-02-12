module MiniWorldWar.Request.Guest exposing
    ( GuestMsg(..)
    , closeGame
    , exit
    , findOldGame
    , findOpenGame
    , joinOpenGame
    , reopenGame
    )

import Dict
import Http exposing (Error(..))
import Json.Decode exposing (Decoder)
import MiniWorldWar.Request as Request
    exposing
        ( OpenGameTable
        , Response(..)
        , RunningGameTable
        , httpDelete
        , openGameRoute
        , runningGameRoute
        )
import Random exposing (Seed)
import Task
import Time exposing (Posix)


type GuestMsg
    = JoinOpenGame String
    | FindOldGame
    | ReopenGame String
    | CreateNewGame
    | JoinGame String
    | CloseGame String
    | FindOpenGame
    | HostGame Seed


type alias GuestResponse =
    Response GuestMsg


exit : Cmd (Response Never)
exit =
    Reset
        |> (Task.perform identity << Task.succeed)



{------------------------
   RunningGame
------------------------}


reopenGame : String -> Cmd GuestResponse
reopenGame id =
    let
        response : Result Error () -> GuestResponse
        response result =
            case result of
                Ok () ->
                    Please CreateNewGame

                Err error ->
                    error |> ExitWithError
    in
    httpDelete
        { url = runningGameRoute ++ "/" ++ id
        , expect = Http.expectWhatever response
        }


findOldGame : Posix -> Cmd GuestResponse
findOldGame now =
    let
        oneHour : Int
        oneHour =
            1000 * 60 * 60

        response : Result Error RunningGameTable -> GuestResponse
        response result =
            case result of
                Ok dict ->
                    case
                        dict
                            |> Dict.filter
                                (\_ { lastUpdated } ->
                                    lastUpdated + oneHour <= (now |> Time.posixToMillis)
                                )
                            |> Dict.toList
                    of
                        ( id, _ ) :: _ ->
                            Please <| ReopenGame id

                        [] ->
                            Please <| CreateNewGame

                Err error ->
                    case error of
                        BadBody _ ->
                            error |> DropRunningGameTable

                        _ ->
                            (error |> Debug.log "error:")
                                |> always (Please <| CreateNewGame)
    in
    Http.get
        { url = runningGameRoute
        , expect = Http.expectJson response Request.runningGameTableDecoder
        }



{------------------------
   OpenGame
------------------------}


openGameTableDecoder : Decoder OpenGameTable
openGameTableDecoder =
    Request.tableDecoder <| Request.openGameDecoder


closeGame : String -> Cmd GuestResponse
closeGame id =
    let
        response : Result Error () -> GuestResponse
        response result =
            case result of
                Ok () ->
                    Please <| FindOpenGame
                Err error ->
                    error |> ResetWithError
    in
    httpDelete
        { url = openGameRoute ++ "/" ++ id
        , expect = Http.expectWhatever response
        }


joinOpenGame : String -> Cmd GuestResponse
joinOpenGame id =
    let
        response : Result Error () -> GuestResponse
        response result =
            case result of
                Ok () ->
                    Please <| JoinGame id

                Err error ->
                    error |> ResetWithError
    in
    httpDelete
        { url = openGameRoute ++ "/" ++ id
        , expect = Http.expectWhatever response
        }

findOpenGame : Posix -> Cmd GuestResponse
findOpenGame now =
    let
        oneHour : Int
        oneHour =
            1000 * 60 * 60

        response : Result Error OpenGameTable -> GuestResponse
        response result =
            case result of
                Ok dict ->
                    case dict |> Dict.toList of
                        ( id, { date } ) :: _ ->
                            if date + oneHour > (now |> Time.posixToMillis) then
                                Please <| JoinOpenGame id

                            else
                                Please <| CloseGame id

                        [] ->
                            Please <| FindOldGame

                Err error ->
                    case error of
                        BadBody _ ->
                            error |> DropOpenGameTable

                        _ ->
                            (error |> Debug.log "error:")
                                |> always (Please <| FindOldGame)
    in
    Http.get
        { url = openGameRoute
        , expect = Http.expectJson response openGameTableDecoder
        }
