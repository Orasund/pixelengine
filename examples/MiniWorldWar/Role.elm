module MiniWorldWar.Role exposing (ClientModel, HostModel, WaitingHostModel)

import MiniWorldWar.Data.Color exposing (Color(..))
import MiniWorldWar.Data.Continent exposing (Continent(..))
import MiniWorldWar.Data.Game exposing (Game, GameState(..))
import MiniWorldWar.Request exposing (Response(..))
import Random exposing (Seed)
import Time exposing (Posix)


type alias HostModel =
    ( ClientModel, Seed )


type alias WaitingHostModel =
    ( { time : Posix
      , id : String
      }
    , Seed
    )


type alias ClientModel =
    { game : Game
    , time : Posix
    , playerColor : Color
    , select :
        Maybe
            ( Continent
            , { selected : Int
              , remaining : Int
              }
            )
    , ready : Bool
    , id : String
    }
