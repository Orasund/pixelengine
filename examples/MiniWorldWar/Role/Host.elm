module MiniWorldWar.Role.Host exposing (tick, update)

import MiniWorldWar.Data.Game as Game exposing (Game, GameState(..))
import MiniWorldWar.Role exposing (ClientModel, HostModel, WaitingHostModel)
import MiniWorldWar.Request as Request exposing (Response(..))
import MiniWorldWar.Request.Host as HostRequest exposing (HostMsg(..))
import Time exposing (Posix)
import Random exposing (Generator, Seed)


tick : HostModel -> (Response HostMsg -> msg) -> Posix -> ( HostModel, Cmd msg )
tick ( { ready, id, game } as model, seed ) msgMap time =
  let
                        { lastUpdated, moveBoard } =
                            game
                    in
                    if ready then
                        case game.state of
                            BothReady ->
                                let
                                    ( newGame, newSeed ) =
                                        seed
                                            |> Random.step (game |> Game.nextRound time)
                                in
                                ( ( { model
                                        | game = newGame
                                        , time = time
                                        , ready = False
                                      }
                                    , newSeed
                                    )
                                , HostRequest.submit id newGame
                                    |> Cmd.map msgMap
                                )

                            _ ->
                                ( ( { model | time = time }, seed )
                                , HostRequest.waitingForClient id lastUpdated
                                    |> Cmd.map msgMap
                                )

                    else
                        (( { model | time = time }, seed ), Cmd.none )

update : HostMsg -> HostModel -> (Response HostMsg -> msg) -> ( HostModel, Cmd msg )
update msg (( { game, id, time } as model, seed ) as modelAndSeed) msgMap =
    case msg of
        Submit ->
            let
                newGame =
                    { game
                        | lastUpdated = time |> Time.posixToMillis
                        , state = HostReady
                    }
            in
            ( ( { model
                    | ready = True
                    , game = newGame
                  }
                , seed
                )
            , HostRequest.submit id newGame
                |> Cmd.map msgMap
            )

        UpdateMoveBoard moveBoard ->
            ( ( { model
                    | game =
                        game
                            --outdated Game -we wait for a newer one
                            |> Game.addMoveBoard moveBoard
                            |> (\g -> { g | state = BothReady })
                  }
                , seed
                )
            , Cmd.none
            )

        WaitingForClient ->
            ( modelAndSeed
            , HostRequest.waitingForClient id game.lastUpdated
                |> Cmd.map msgMap
            )