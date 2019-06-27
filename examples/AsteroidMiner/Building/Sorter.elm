module AsteroidMiner.Building.Sorter exposing (canStore, update)

import AsteroidMiner.Building exposing (BuildingType(..))
import AsteroidMiner.Data.Item exposing (Item)
import AsteroidMiner.Data.Map exposing (Command, Neighborhood)
import AsteroidMiner.Lib.Command as Command
import AsteroidMiner.Lib.Neighborhood as Neighborhood


canStore : Neighborhood -> Item -> { value : Int, item : Item } -> Bool
canStore _ _ _ =
    False


update : Neighborhood -> Command
update neigh =
    neigh
        |> Neighborhood.toList
        |> List.filterMap
            (\( dir, a ) ->
                case a of
                    Just (ColoredConveyorBelt _ d) ->
                        if dir == d then
                            Just <| Command.send dir

                        else
                            Nothing

                    Just (Container _) ->
                        Just <| Command.send dir

                    _ ->
                        Nothing
            )
        |> Command.batch
