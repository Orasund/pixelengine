module AsteroidMiner.Data.Map exposing (Command, GroundType(..), Map, Neighborhood, Square, SquareType, init)

import AsteroidMiner.Building exposing (BuildingType)
import AsteroidMiner.Data exposing (size)
import AsteroidMiner.Data.Item exposing (Item)
import AsteroidMiner.Lib.Command as Command
import AsteroidMiner.Lib.Map as Map exposing (SquareType(..))
import AsteroidMiner.Lib.Neighborhood as Neighborhood
import Grid.Bordered as Grid


type GroundType
    = Dirt
    | Mountain
    | OreGround


type alias Neighborhood =
    Neighborhood.Neighborhood (Maybe BuildingType)


type alias Command =
    Command.Command BuildingType Item


type alias SquareType =
    Map.SquareType BuildingType GroundType


type alias Square =
    Map.Square BuildingType GroundType Item


type alias Map =
    Map.Map BuildingType GroundType Item


init : Map
init =
    let
        center : Int
        center =
            size // 2
    in
    Grid.fill
        (\( x, y ) ->
            if (x - center) ^ 2 + (y - center) ^ 2 <= 8 ^ 2 then
                Just <|
                    if (x + 1 - center) ^ 2 + (y - 1 - center) ^ 2 <= 6 ^ 2 then
                        ( GroundSquare <| Mountain, Nothing )

                    else
                        ( GroundSquare <| Dirt, Nothing )

            else
                Nothing
        )
        { rows = size
        , columns = size
        }
