module AsteroidMiner.Data.Map exposing (Command, GroundType(..), Map, Neighborhood, Square, SquareType, init, update)

import AsteroidMiner.Building exposing (BuildingType(..), Volume(..))
import AsteroidMiner.Data exposing (size)
import AsteroidMiner.Data.Item exposing (Item)
import AsteroidMiner.Lib.Command as Command
import AsteroidMiner.Lib.Map as Map exposing (SquareType(..))
import AsteroidMiner.Lib.Neighborhood as Neighborhood
import Grid.Bordered as Grid
import Grid.Position as Position exposing (Position)


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


update : { empty : GroundType, update : Position -> Command, canStore : Position -> BuildingType -> Item -> { value : Int, item : Item } -> Bool } -> Map -> ( Map, Int )
update fun map =
    map
        |> Grid.foldl
            (\pos maybeSquare ( m, inv ) ->
                case maybeSquare of
                    Just (( BuildingSquare { sort, value }, maybeItem ) as square) ->
                        ( m |> Map.apply (fun.update pos) pos square { empty = fun.empty, lookUp = map, canStore = fun.canStore }
                        , inv
                            |> (case maybeItem of
                                    Just _ ->
                                        case sort of
                                            Container Empty ->
                                                identity

                                            Container _ ->
                                                (+) (1 + value)

                                            _ ->
                                                identity

                                    _ ->
                                        identity
                               )
                        )

                    _ ->
                        ( m
                        , inv
                        )
            )
            ( map, 0 )
