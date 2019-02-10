module MiniWorldWar.Server.WaitingHost exposing (
    WaitingHostResponse(..),
    hostGame,checkForOpponent,stopWaitingForOpponent)

import Http exposing (Error(..), Expect)
import Json.Encode as E exposing (Value)
import Json.Decode as D exposing (Decoder)
import MiniWorldWar.Game as Game exposing (Game)
import MiniWorldWar.Server as Server exposing (httpDelete, openGameRoute, runningGameRoute)
import Time exposing (Posix)


type WaitingHostResponse
    = WaitForOpponent
    | StopWaitingForOpponent
    | CreateBoard Game
    | FailedToHost
    | UpdateAsWaitingHost Posix
    | IdleAsWaitingHost

{------------------------
   RunningGame
------------------------}

stopWaitingForOpponent : String -> Cmd WaitingHostResponse
stopWaitingForOpponent id =
    let
        response : Result Error () -> WaitingHostResponse
        response result =
            FailedToHost
    in
    httpDelete
      { url = runningGameRoute ++ "/" ++ id
      , expect = Http.expectWhatever response
      }

checkForOpponent : String -> Cmd WaitingHostResponse
checkForOpponent id =
  let
      response : Result Error (Maybe Game) -> WaitingHostResponse
      response result =
          case result of
              Ok maybeGame ->
                case maybeGame of
                    Nothing ->
                        IdleAsWaitingHost
                    Just game ->
                        CreateBoard game

              Err error ->
                    case error of
                        BadBody _ ->
                            (error |> Debug.log "badBodyError:")
                            |> always StopWaitingForOpponent
                        _ ->
                            (error |> Debug.log "error:")
                            |> always StopWaitingForOpponent
  in
  Http.get
    { url = runningGameRoute ++ "/" ++ id
    , expect = Http.expectJson response (D.field "result" <| D.nullable <| Game.decoder)
    }

{------------------------
   OpenGame
------------------------}

encodeOpenGame : { date : Int } -> Value
encodeOpenGame { date } =
    E.object
        [ ( "date", E.int date )
        ]

hostGame : String -> Posix -> Cmd WaitingHostResponse
hostGame id time =
    let
        value : Value
        value =
            encodeOpenGame { date = time |> Time.posixToMillis }

        response : Result Error () -> WaitingHostResponse
        response result =
            case result of
                Ok () ->
                    WaitForOpponent

                Err error ->
                    case error of
                        BadBody _->
                            (error |> Debug.log "badBodyError:")
                            |> always FailedToHost
                        _ ->
                            (error |> Debug.log "error:")
                            |> always FailedToHost
    in
    Http.post
        { url = openGameRoute ++ "/" ++ id
        , body = Http.jsonBody value
        , expect = Http.expectWhatever response
        }
