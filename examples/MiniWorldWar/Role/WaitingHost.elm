module MiniWorldWar.Role.WaitingHost exposing (tick, update)

import MiniWorldWar.Data.Color exposing (Color(..))
import MiniWorldWar.Data.Game as Game exposing (GameState(..))
import MiniWorldWar.Request exposing (Response(..))
import MiniWorldWar.Request.Host as HostRequest exposing (HostMsg(..))
import MiniWorldWar.Request.WaitingHost as WaitingHostRequest exposing (WaitingHostMsg(..))
import MiniWorldWar.Role exposing (HostModel, WaitingHostModel)
import Random exposing (Seed)
import Time exposing (Posix)


tick : WaitingHostModel -> (Response WaitingHostMsg -> msg) -> Posix -> ( WaitingHostModel, Cmd msg )
tick ( { id }, seed ) msgMap time =
    ( ( { id = id, time = time }, seed )
    , WaitingHostRequest.checkForOpponent id
        |> Cmd.map msgMap
    )


update :
    WaitingHostMsg
    -> ( { time : Posix, id : String }, Seed )
    -> (WaitingHostModel -> state)
    -> (HostModel -> state)
    -> (Response WaitingHostMsg -> msg)
    -> (Response HostMsg -> msg)
    -> ( state, Cmd msg )
update msg (( { time, id }, seed ) as waitingHostModel) waitingHostModelMap hostModelMap waitingHostMsgMap hostMsgMap =
    case msg of
        WaitForOpponent ->
            ( waitingHostModelMap waitingHostModel
            , WaitingHostRequest.checkForOpponent id
                |> Cmd.map waitingHostMsgMap
            )

        CreateBoard game ->
            let
                ( newGame, newSeed ) =
                    seed
                        |> Random.step
                            (game |> Game.nextRound time)
            in
            ( hostModelMap
                ( { time = time
                  , id = id
                  , game = newGame
                  , select = Nothing
                  , playerColor = Red
                  , ready = False
                  }
                , newSeed
                )
            , newGame
                |> HostRequest.submit id
                |> Cmd.map hostMsgMap
            )
