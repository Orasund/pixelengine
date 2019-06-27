module AsteroidMiner.Building exposing (BeltColor(..), Building, BuildingType(..), Code(..), Volume(..), canBreak, isColoredConveyorBelt, isConveyorBelt, isConveyorBeltColored, isInput, isOutput)

import AsteroidMiner.View exposing (ToolSelection(..))
import Grid.Direction exposing (Direction)


type alias Building a =
    { value : Int
    , sort : a
    }


type BeltColor
    = Red
    | Blue
    | Green
    | Yellow


type Volume
    = Empty
    | HalfEmpty
    | HalfFull
    | Full


type Code
    = Invalid
    | InputFound
    | Try BeltColor
    | Failed BeltColor


type BuildingType
    = Mine
    | ConveyorBelt Code
    | ColoredConveyorBelt BeltColor Direction
    | Container Volume
    | Merger
    | Sorter


isOutput : BuildingType -> Bool
isOutput sort =
    case sort of
        Mine ->
            True

        Merger ->
            True

        _ ->
            False


isInput : BuildingType -> Bool
isInput sort =
    case sort of
        Container _ ->
            True

        Sorter ->
            True

        _ ->
            False


canBreak : BuildingType -> Bool
canBreak sort =
    case sort of
        Container Empty ->
            True

        Container _ ->
            False

        Mine ->
            False

        _ ->
            True


isConveyorBelt : BuildingType -> Bool
isConveyorBelt sort =
    case sort of
        ConveyorBelt _ ->
            True

        _ ->
            False


isColoredConveyorBelt : BuildingType -> Bool
isColoredConveyorBelt sort =
    case sort of
        ColoredConveyorBelt _ _ ->
            True

        _ ->
            False


isConveyorBeltColored : BeltColor -> BuildingType -> Bool
isConveyorBeltColored color sort =
    case sort of
        ColoredConveyorBelt c _ ->
            c == color

        _ ->
            False
