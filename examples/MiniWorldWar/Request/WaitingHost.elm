module MiniWorldWar.Request.WaitingHost exposing
    ( WaitingHostMsg(..)
    , checkForOpponent
    , exit
    , hostGame
    )

import Http exposing (Error(..))
import Json.Decode as D
import Json.Encode exposing (Value)
import MiniWorldWar.Data.Game as Game exposing (Game)
import MiniWorldWar.Request as Request exposing (Response(..), httpDelete, openGameRoute, runningGameRoute)
import Time exposing (Posix)


type WaitingHostMsg
    = WaitForOpponent
    | CreateBoard Game


type alias WaitingHostResponse =
    Response WaitingHostMsg


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


checkForOpponent : String -> Cmd WaitingHostResponse
checkForOpponent id =
    let
        response : Result Error (Maybe Game) -> WaitingHostResponse
        response result =
            case result of
                Ok maybeGame ->
                    case maybeGame of
                        Nothing ->
                            Idle

                        Just game ->
                            Please <| CreateBoard game

                Err error ->
                    error |> ExitWithError
    in
    Http.get
        { url = runningGameRoute ++ "/" ++ id
        , expect = Http.expectJson response (D.field "result" <| D.nullable <| Game.decoder)
        }



{------------------------
   OpenGame
------------------------}


hostGame : String -> Posix -> Cmd WaitingHostResponse
hostGame id time =
    let
        value : Value
        value =
            Request.openGameEncoder { date = time |> Time.posixToMillis }

        response : Result Error () -> WaitingHostResponse
        response result =
            case result of
                Ok () ->
                    Please WaitForOpponent

                Err error ->
                    error |> ResetWithError
    in
    Http.post
        { url = openGameRoute ++ "/" ++ id
        , body = Http.jsonBody value
        , expect = Http.expectWhatever response
        }
