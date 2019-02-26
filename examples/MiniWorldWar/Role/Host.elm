module MiniWorldWar.Role.Host exposing (tick, update)

import MiniWorldWar.Data.Game as Game exposing (GameState(..))
import MiniWorldWar.Request exposing (Response(..))
import MiniWorldWar.Request.Host as HostRequest exposing (HostMsg(..))
import MiniWorldWar.Role exposing (HostModel)
import Random
import Time exposing (Posix)


tick : HostModel -> (Response HostMsg -> msg) -> Posix -> ( HostModel, Cmd msg )
tick ( { ready, id, game } as model, seed ) msgMap time =
    let
        { lastUpdated } =
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
        ( ( { model | time = time }, seed ), Cmd.none )


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
