module MiniWorldWar.Role.WaitingHost exposing (tick,update)

import MiniWorldWar.Data.Game as Game exposing (Game, GameState(..))
import MiniWorldWar.Role exposing (ClientModel, HostModel, WaitingHostModel)
import MiniWorldWar.Request as Request exposing (Response(..))
import MiniWorldWar.Request.Host as HostRequest exposing (HostMsg(..))
import MiniWorldWar.Request.WaitingHost as WaitingHostRequest exposing (WaitingHostMsg(..))
import Time exposing (Posix)
import Random exposing (Generator, Seed)
import MiniWorldWar.Data.Color as Color exposing (Color(..))

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
