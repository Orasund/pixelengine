module MiniWorldWar.Role.Client exposing (init, tick, update)

import Action
import MiniWorldWar.Data.Color exposing (Color(..))
import MiniWorldWar.Data.Game as Game exposing (Game, GameState(..))
import MiniWorldWar.Request exposing (Response(..))
import MiniWorldWar.Request.Client as ClientRequest exposing (ClientMsg(..))
import MiniWorldWar.Role exposing (ClientModel)
import Time exposing (Posix)


type alias Action =
    Action.Action ClientModel (Response ClientMsg) Never Never


type alias Flags =
    { time : Posix
    , id : String
    }


init : Flags -> ( ClientModel, Cmd (Response ClientMsg) )
init { time, id } =
    let
        game : Game
        game =
            time |> Time.posixToMillis |> Game.new
    in
    ( { game = game
      , time = time
      , playerColor = Blue
      , select = Nothing
      , ready = True
      , id = id
      }
    , ClientRequest.joinGame id game
    )


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


update : ClientMsg -> ClientModel -> Action
update msg ({ time, id, game } as model) =
    case msg of
        WaitingForHost ->
            Action.updating
                ( model
                , ClientRequest.waitingForHost id game.lastUpdated
                )

        UpdateGame ({ moveBoard } as newGame) ->
            case newGame.state of
                Running ->
                    Action.updating
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
                    Action.updating
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
                        )

                _ ->
                    Action.updating
                        ( { model | game = newGame, ready = False }
                        , ClientRequest.endGame id time
                        )

        EndGame ->
            Action.updating
                ( model
                , ClientRequest.endGame id time
                )

        UpdateGameTable table ->
            Action.updating
                ( model
                , ClientRequest.updateGameTable table
                )

        Ready ->
            Action.updating
                ( { model
                    | ready = True
                  }
                , Cmd.none
                )
