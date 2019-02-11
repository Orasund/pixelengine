module MiniWorldWar.Role.Guest exposing (update)

import MiniWorldWar.Data.Game as Game exposing (Game, GameState(..))
import MiniWorldWar.Role exposing ( ClientModel, HostModel, WaitingHostModel)
import MiniWorldWar.Request as Request exposing (Response(..))
import MiniWorldWar.Request.Guest as GuestRequest exposing (GuestMsg(..))
import MiniWorldWar.Request.Client as ClientRequest exposing (ClientMsg(..))
import MiniWorldWar.Request.WaitingHost as WaitingHostRequest exposing (WaitingHostMsg(..))
import Time exposing (Posix)
import Random exposing (Generator, Seed)
import MiniWorldWar.Data.Color as Color exposing (Color(..))

update :
    GuestMsg
    -> Posix
    -> (Posix -> state)
    -> (WaitingHostModel -> state)
    -> (ClientModel -> state)
    -> (Response GuestMsg -> msg)
    -> (Response WaitingHostMsg -> msg)
    -> (Response ClientMsg -> msg)
    -> ( state, Cmd msg )
update msg time guestModelMap waitingHostModelMap clientModelMap guestMsgMap waitingHostMsgMap clientMsgMap=
    let
        game : Game
        game =
            time |> Time.posixToMillis |> Game.new
    in
    case msg of
                JoinGame id ->
                    ( clientModelMap
                        { game = game
                        , time = time
                        , id = id
                        , select = Nothing
                        , playerColor = Blue
                        , ready = True
                        }
                    , ClientRequest.joinGame id game
                        |> Cmd.map clientMsgMap
                    )

                JoinOpenGame id ->
                    ( guestModelMap time
                    , GuestRequest.joinOpenGame id
                        |> Cmd.map guestMsgMap
                    )

                CloseGame id ->
                    ( guestModelMap time
                    , GuestRequest.closeGame id
                        |> Cmd.map guestMsgMap
                    )

                ReopenGame id ->
                    ( guestModelMap time
                    , GuestRequest.reopenGame id
                        |> Cmd.map guestMsgMap
                    )

                FindOldGame ->
                    ( guestModelMap time
                    , GuestRequest.findOldGame time
                        |> Cmd.map guestMsgMap
                    )

                CreateNewGame ->
                    ( guestModelMap time
                    , Random.generate
                        (guestMsgMap << Please << HostGame)
                        Random.independentSeed
                    )

                FindOpenGame ->
                    ( guestModelMap time
                    , GuestRequest.findOpenGame time
                        |> Cmd.map guestMsgMap
                    )

                HostGame seed ->
                    let
                        ( id, newSeed ) =
                            Random.step
                                (Random.int Random.minInt Random.maxInt
                                    |> Random.map String.fromInt
                                )
                                seed
                    in
                    ( waitingHostModelMap
                        ( { time = time
                          , id = id
                          }
                        , newSeed
                        )
                    , WaitingHostRequest.hostGame id time
                        |> Cmd.map waitingHostMsgMap
                    )
