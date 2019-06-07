module MiniWorldWar.Role.WaitingHost exposing (init, tick, update)

import Action
import MiniWorldWar.Data.Color exposing (Color(..))
import MiniWorldWar.Data.Game exposing (GameState(..))
import MiniWorldWar.Request exposing (Response(..))
import MiniWorldWar.Request.WaitingHost as WaitingHostRequest exposing (WaitingHostMsg(..))
import MiniWorldWar.Role exposing (WaitingHostModel)
import MiniWorldWar.Role.Host as Host
import Random exposing (Seed)
import Time exposing (Posix)


type alias Action =
    Action.Action WaitingHostModel (Response WaitingHostMsg) Host.TransitionData Never


init : WaitingHostModel -> ( WaitingHostModel, Cmd (Response WaitingHostMsg) )
init (( { id, time }, _ ) as waitingHostModel) =
    ( waitingHostModel
    , WaitingHostRequest.hostGame id time
    )


tick : WaitingHostModel -> Posix -> Action.Action WaitingHostModel (Response WaitingHostMsg) Never Never
tick ( { id }, seed ) time =
    Action.updating
        ( ( { id = id, time = time }, seed )
        , WaitingHostRequest.checkForOpponent id
        )


update : WaitingHostMsg -> ( { time : Posix, id : String }, Seed ) -> Action
update msg (( { time, id }, seed ) as waitingHostModel) =
    case msg of
        WaitForOpponent ->
            Action.updating
                ( waitingHostModel
                , WaitingHostRequest.checkForOpponent id
                )

        CreateBoard game ->
            Action.transitioning
                { game = game, seed = seed, time = time, id = id }
