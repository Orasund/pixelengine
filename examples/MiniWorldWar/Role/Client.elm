module MiniWorldWar.Role.Client exposing (tick, update)

import MiniWorldWar.Data.Game as Game exposing (Game, GameState(..))
import MiniWorldWar.Request as Request exposing (Response(..))
import MiniWorldWar.Request.Client as ClientRequest exposing (ClientMsg(..))
import MiniWorldWar.Role exposing (ClientModel, HostModel, WaitingHostModel)
import Time exposing (Posix)


tick : ClientModel -> (Response ClientMsg -> msg) -> Posix -> ( ClientModel, Cmd msg )
tick ({ game, id, ready } as model) msgMap time =
    ( { model | time = time }
    , let
        { lastUpdated } =
            game
      in
      if ready then
        ClientRequest.waitingForHost id lastUpdated
            |> Cmd.map msgMap

      else
        Cmd.none
    )


update : ClientMsg -> ClientModel -> (Response ClientMsg -> msg) -> ( ClientModel, Cmd msg )
update msg ({ time, id, game } as model) msgMap =
    case msg of
        WaitingForHost ->
            ( model
            , ClientRequest.waitingForHost id game.lastUpdated
                |> Cmd.map msgMap
            )

        UpdateGame ({ moveBoard } as newGame) ->
            case newGame.state of
                Running ->
                    ( { model
                        | ready = False
                        , game = newGame
                      }
                    , Cmd.none
                    )

                HostReady ->
                    let
                        newerGame =
                            { game
                                | lastUpdated = time |> Time.posixToMillis
                                , state = BothReady
                            }
                    in
                    ( { model
                        | game =
                            game
                                --outdated Game -we wait for a newer one
                                |> Game.addMoveBoard moveBoard
                                |> (\g ->
                                        { g
                                            | state = BothReady
                                            , lastUpdated = time |> Time.posixToMillis
                                        }
                                   )
                      }
                    , ClientRequest.submitMoveBoard newerGame id
                        |> Cmd.map msgMap
                    )

                _ ->
                    ( { model | game = newGame, ready = False }
                    , ClientRequest.endGame id time
                        |> Cmd.map msgMap
                    )

        EndGame ->
            ( model
            , ClientRequest.endGame id time
                |> Cmd.map msgMap
            )

        UpdateGameTable table ->
            ( model
            , ClientRequest.updateGameTable table
                |> Cmd.map msgMap
            )

        Ready ->
            ( { model
                | ready = True
              }
            , Cmd.none
            )
