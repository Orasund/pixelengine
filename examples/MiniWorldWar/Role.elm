module MiniWorldWar.Role exposing (HostModel,ClientModel,WaitingHostModel)

import Random exposing (Generator, Seed)
import Time exposing (Posix)
import MiniWorldWar.Data.Game as Game exposing (Game, GameState(..))
import MiniWorldWar.Data.Color as Color exposing (Color(..))
import MiniWorldWar.Data.Continent as Continent exposing (Continent(..))
import MiniWorldWar.Request as Request exposing (Response(..))

type alias HostModel =
  (ClientModel,Seed)

type alias WaitingHostModel =
  ({ time : Posix, id : String},Seed)

type alias ClientModel =
    { game : Game
    , time : Posix
    , playerColor : Color
    , select : Maybe ( Continent, { selected : Int, remaining : Int } )
    , ready : Bool
    , id : String
    }