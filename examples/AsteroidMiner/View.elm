module AsteroidMiner.View exposing (ToolSelection(..))

import AsteroidMiner.Data.Item exposing (Item)


type ToolSelection
    = Mine
    | ConveyorBelt
    | Container
    | Merger
    | Sorter
    | Floor
    | Delete
    | Bag (Maybe Item)
