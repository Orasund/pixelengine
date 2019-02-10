module MiniWorldWar.Server.Guest exposing
    ( GuestResponse(..)
    , findOpenGame
    , joinOpenGame
    , closeGame
    , reopenGame
    , findOldGame
    , dropOpenGameTable
    , dropRunningGameTable
    )

import Http exposing (Error(..), Expect)
import Json.Decode as D exposing (Decoder)
import MiniWorldWar.Game as Game exposing (Game)
import MiniWorldWar.Server as Server exposing (
        OpenGameTable,
        RunningGameTable,
        httpDelete, openGameRoute, runningGameRoute)
import Random exposing (Seed)
import Time exposing (Posix)
import Dict exposing (Dict)

type GuestResponse
    = JoinOpenGame String
    | CloseGame String
    | FindOldGame
    | ReopenGame String
    | CreateNewGame
    | JoinGame String
    | FindOpenGame
    | GameNotFound
    | DropRunningGameTable
    | DropOpenGameTable



{------------------------
   RunningGame
------------------------}

reopenGame : String -> Cmd GuestResponse
reopenGame id =
    let
        response : Result Error () -> GuestResponse
        response result =
            CreateNewGame
    in
    httpDelete
        { url = runningGameRoute ++ "/" ++ id
        , expect = Http.expectWhatever response
        }

dropRunningGameTable : Cmd GuestResponse
dropRunningGameTable =
    let
        response : Result Error () -> GuestResponse
        response result =
            CreateNewGame
    in
    httpDelete
        { url = runningGameRoute
        , expect = Http.expectWhatever response
        }


findOldGame : Posix -> Cmd GuestResponse
findOldGame now =
    let
        oneHour : Int
        oneHour = 1000*60*60

        response : Result Error RunningGameTable -> GuestResponse
        response result =
            case result of
                Ok dict ->
                  case
                    dict
                    |> Dict.filter
                        (\_ {lastUpdated}->
                            lastUpdated + oneHour <= (now |> Time.posixToMillis)
                        )
                    |> Dict.toList

                  of
                    ((id,_) :: _) ->
                      ReopenGame id

                    [] ->
                      CreateNewGame

                Err error ->
                    case error of
                        BadBody _->
                            (error |> Debug.log "badBodyError:")
                                |> always DropRunningGameTable
                        _ ->
                            (error |> Debug.log "error:")
                                |> always CreateNewGame
    in
    Http.get
        { url = runningGameRoute
        , expect = Http.expectJson response Server.runningGameTableDecoder
        }

{------------------------
   OpenGame
------------------------}



openGameTableDecoder : Decoder OpenGameTable
openGameTableDecoder =
    Server.tableDecoder <| Server.openGameDecoder

closeGame : String -> Cmd GuestResponse
closeGame id =
    let
        response : Result Error () -> GuestResponse
        response result =
            GameNotFound
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
                    JoinGame id

                Err error ->
                    case error of
                        BadBody _->
                            (error |> Debug.log "BadBodyError:")
                                |> always GameNotFound
                        _ ->
                            (error |> Debug.log "error:")
                                |> always GameNotFound
    in
    httpDelete
        { url = openGameRoute ++ "/" ++ id
        , expect = Http.expectWhatever response
        }

dropOpenGameTable : Cmd GuestResponse
dropOpenGameTable =
    let
        response : Result Error () -> GuestResponse
        response result =
            FindOldGame
    in
    httpDelete
        { url = openGameRoute
        , expect = Http.expectWhatever response
        }

findOpenGame : Posix -> Cmd GuestResponse
findOpenGame now =
    let
        oneHour : Int
        oneHour = 1000*60*60

        response : Result Error OpenGameTable -> GuestResponse
        response result =
            case result of
                Ok dict ->
                  case dict |> Dict.toList of
                    ((id,{date}) :: _) ->
                      if date + oneHour > (now |> Time.posixToMillis) then
                        JoinOpenGame id
                      else
                        CloseGame id

                    [] ->
                      FindOldGame

                Err error ->
                    case error of
                        BadBody _->
                            (error |> Debug.log "badBodyError:")
                                |> always DropOpenGameTable
                        _ ->
                            (error |> Debug.log "error:")
                                |> always FindOldGame
    in
    Http.get
        { url = openGameRoute
        , expect = Http.expectJson response openGameTableDecoder
        }
