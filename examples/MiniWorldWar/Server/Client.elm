module MiniWorldWar.Server.Client exposing (
  ClientMsg(..),
  joinGame,
  waitingForHost,
  exitGame
  )

import MiniWorldWar.Server as Server exposing (Response, httpDelete, runningGameRoute)
import MiniWorldWar.Game as Game exposing (Game)
import Http exposing (Expect,Error(..))
import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
import Time exposing (Posix)

type ClientMsg =
  WaitingForHost
  | UpdateGame Game
  | ExitGame
  | IdleAsClient
  | ResetAsClient
  | DropRunningGameTableAsClient

type alias ClientResponse =
    Response ClientMsg

{------------------------
   RunningGame
------------------------}
dropRunningGameTableAsClient : Cmd ClientResponse
dropRunningGameTableAsClient =
    let
        response : Result Error () -> ClientResponse
        response result =
            ResetAsClient
    in
    httpDelete
        { url = runningGameRoute
        , expect = Http.expectWhatever response
        }

exitGame : String -> Cmd ClientResponse
exitGame id =
    let
        response : Result Error () -> ClientResponse
        response result =
            ResetAsClient
    in
    httpDelete
      { url = runningGameRoute ++ "/" ++ id
      , expect = Http.expectWhatever response
      }

waitingForHost : String -> Int -> Cmd ClientResponse
waitingForHost id oldTime =
  let
      response : Result Error (Game) -> ClientResponse
      response result =
          case result of
              Ok ({lastUpdated} as game) ->
                if lastUpdated > oldTime then
                    UpdateGame game
                else
                    IdleAsClient

              Err error ->
                    case error of
                        BadBody _ ->
                            (error |> Debug.log "badBodyError:")
                            |> always ExitGame
                        _ ->
                            (error |> Debug.log "error:")
                            |> always DropRunningGameTableAsClient
  in
  Http.get
    { url = runningGameRoute ++ "/" ++ id
    , expect = Http.expectJson response (D.field "result" <| Game.decoder)
    }

joinGame : String -> Game -> Cmd ClientResponse
joinGame id game=
    let
        value : Value
        value =
            Game.encode game

        response : Result Error () -> ClientResponse
        response result =
            case result of
                Ok () ->
                    WaitingForHost

                Err _ ->
                    ResetAsClient
    in
    Http.post
        { url = runningGameRoute ++ "/" ++ id
        , body = Http.jsonBody value
        , expect = Http.expectWhatever response
        }