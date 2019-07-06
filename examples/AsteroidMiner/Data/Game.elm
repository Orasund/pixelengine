module AsteroidMiner.Data.Game exposing (Game, emptySquare, getBuildingType, getGroundType, isBuildingType, isGroundType, isValid, newBuilding, solveConflict, updateBuilding)

import AsteroidMiner.Building as Building exposing (BuildingType(..), Volume(..))
import AsteroidMiner.Building.ColoredConveyorBelt as ColoredConveyorBelt
import AsteroidMiner.Building.Container as Container
import AsteroidMiner.Building.ConveyorBelt as ConveyorBelt
import AsteroidMiner.Building.Merger as Merger
import AsteroidMiner.Building.Mine as Mine
import AsteroidMiner.Building.Sorter as Sorter
import AsteroidMiner.Data exposing (mineVolume)
import AsteroidMiner.Data.Comet exposing (Comet)
import AsteroidMiner.Data.Item as Item exposing (Item)
import AsteroidMiner.Data.Map as Map exposing (GroundType(..), Map, Neighborhood, Square)
import AsteroidMiner.Lib.Map as Map exposing (SquareType(..))
import AsteroidMiner.Lib.Neighborhood as Neighborhood
import AsteroidMiner.View as View exposing (ToolSelection)
import Dict exposing (Dict)
import Grid.Bordered as Grid
import Grid.Position exposing (Position)


type alias Game =
    { comet : Comet
    , map : Map
    , bag : Maybe Item
    , debts : Int
    }


solveConflict : BuildingType -> Neighborhood -> Item -> { item : Item, value : Int } -> Bool
solveConflict sort neigh =
    case sort of
        ColoredConveyorBelt _ _ ->
            ColoredConveyorBelt.canStore neigh

        ConveyorBelt _ ->
            ConveyorBelt.canStore neigh

        Mine ->
            Mine.canStore neigh

        Container _ ->
            Container.canStore neigh

        Merger ->
            Merger.canStore neigh

        Sorter ->
            Sorter.canStore neigh


updateBuilding : BuildingType -> ({ value : Int, item : Maybe Item } -> Neighborhood -> Map.Command)
updateBuilding sort =
    case sort of
        ColoredConveyorBelt color dir ->
            always <| ColoredConveyorBelt.update color dir

        ConveyorBelt code ->
            always <| ConveyorBelt.update code

        Mine ->
            Mine.update

        Container bool ->
            Container.update bool

        Merger ->
            always <| Merger.update

        Sorter ->
            always <| Sorter.update


newBuilding : Maybe Item -> BuildingType -> Square
newBuilding maybeItem buildingType =
    let
        value : Int
        value =
            case buildingType of
                Building.Mine ->
                    mineVolume

                _ ->
                    0
    in
    ( BuildingSquare { value = value, sort = buildingType }, maybeItem )


emptySquare : Maybe Item -> Square
emptySquare maybeItem =
    ( GroundSquare Dirt, maybeItem )


getBuildingType : Square -> Maybe BuildingType
getBuildingType square =
    case square of
        ( BuildingSquare { sort }, _ ) ->
            Just sort

        _ ->
            Nothing


isBuildingType : BuildingType -> Square -> Bool
isBuildingType bType =
    getBuildingType
        >> Maybe.map ((==) bType)
        >> Maybe.withDefault False


getGroundType : Square -> Maybe GroundType
getGroundType square =
    case square of
        ( GroundSquare g, _ ) ->
            Just g

        _ ->
            Nothing


isGroundType : GroundType -> Square -> Bool
isGroundType groundType =
    getGroundType
        >> Maybe.map ((==) groundType)
        >> Maybe.withDefault False


isValidMinePos : Neighborhood.Neighborhood (Maybe Square) -> Bool
isValidMinePos neigh =
    [ neigh.up, neigh.left, neigh.right, neigh.down ]
        |> List.any
            (Maybe.map
                (Tuple.first
                    >> ((/=) <| GroundSquare Mountain)
                )
                >> Maybe.withDefault False
            )


isValid : ToolSelection -> Position -> Map -> Bool
isValid selected position map =
    case map |> Neighborhood.fromPosition position of
        Ok ( Just square, neigh ) ->
            case ( selected, square ) of
                ( View.Floor, _ ) ->
                    False

                ( View.Delete, ( GroundSquare _, _ ) ) ->
                    False

                ( View.Bag Nothing, ( GroundSquare _, Just _ ) ) ->
                    True

                ( View.Bag Nothing, ( GroundSquare _, Nothing ) ) ->
                    False

                ( View.Bag (Just _), ( GroundSquare _, _ ) ) ->
                    False

                ( View.Mine, ( GroundSquare Dirt, _ ) ) ->
                    False

                ( _, ( GroundSquare Dirt, _ ) ) ->
                    True

                ( View.Mine, ( GroundSquare Mountain, _ ) ) ->
                    neigh |> isValidMinePos

                ( _, ( GroundSquare Mountain, _ ) ) ->
                    False

                ( View.Delete, ( BuildingSquare { sort }, _ ) ) ->
                    sort |> Building.canBreak

                ( View.Bag (Just a), ( BuildingSquare { sort, value }, Just b ) ) ->
                    if solveConflict sort (neigh |> Neighborhood.map (Maybe.andThen getBuildingType)) a { item = b, value = value } then
                        sort |> Building.isInput

                    else
                        False

                ( _, _ ) ->
                    False

        Ok ( Nothing, neigh ) ->
            (selected == View.Floor)
                && (neigh
                        |> Neighborhood.toList
                        |> List.any (Tuple.second >> (/=) Nothing)
                   )

        _ ->
            False
