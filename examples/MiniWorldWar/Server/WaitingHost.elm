module MiniWorldWar.Server.WaitingHost exposing
    ( WaitingHostMsg(..)
    , checkForOpponent
    , exit
    , hostGame
    )

import Http exposing (Error(..), Expect)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
import MiniWorldWar.Game as Game exposing (Game)
import MiniWorldWar.Server as Server exposing (Response(..), httpDelete, openGameRoute, runningGameRoute)
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
            Reset
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
                    case error of
                        BadBody _ ->
                            (error |> Debug.log "badBodyError:")
                                |> always Exit

                        _ ->
                            (error |> Debug.log "error:")
                                |> always Exit
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
            Server.openGameEncoder { date = time |> Time.posixToMillis }

        response : Result Error () -> WaitingHostResponse
        response result =
            case result of
                Ok () ->
                    Please WaitForOpponent

                Err error ->
                    case error of
                        BadBody _ ->
                            (error |> Debug.log "badBodyError:")
                                |> always Reset

                        _ ->
                            (error |> Debug.log "error:")
                                |> always Reset
    in
    Http.post
        { url = openGameRoute ++ "/" ++ id
        , body = Http.jsonBody value
        , expect = Http.expectWhatever response
        }
