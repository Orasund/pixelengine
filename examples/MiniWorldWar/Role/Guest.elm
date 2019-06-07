module MiniWorldWar.Role.Guest exposing (update)

import Action
import MiniWorldWar.Data.Color exposing (Color(..))
import MiniWorldWar.Data.Game exposing (GameState(..))
import MiniWorldWar.Request exposing (Response(..))
import MiniWorldWar.Request.Guest as GuestRequest exposing (GuestMsg(..))
import MiniWorldWar.Request.WaitingHost exposing (WaitingHostMsg(..))
import Random exposing (Seed)
import Time exposing (Posix)


type alias TransitionData =
    { id : String
    , maybeSeed : Maybe Seed
    }


type alias Action =
    Action.Action Posix (Response GuestMsg) TransitionData Never


update :
    GuestMsg
    -> Posix
    -> Action
update msg time =
    {- -}
    case msg of
        JoinGame id ->
            Action.transitioning
                { id = id
                , maybeSeed = Nothing
                }

        JoinOpenGame id ->
            Action.updating
                ( time
                , GuestRequest.joinOpenGame id
                )

        CloseGame id ->
            Action.updating
                ( time
                , GuestRequest.closeGame id
                )

        ReopenGame id ->
            Action.updating
                ( time
                , GuestRequest.reopenGame id
                )

        FindOldGame ->
            Action.updating
                ( time
                , GuestRequest.findOldGame time
                )

        CreateNewGame ->
            Action.updating
                ( time
                , Random.generate
                    (Please << HostGame)
                    Random.independentSeed
                )

        FindOpenGame ->
            Action.updating
                ( time
                , GuestRequest.findOpenGame time
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
            Action.transitioning
                { id = id
                , maybeSeed = Just newSeed
                }
