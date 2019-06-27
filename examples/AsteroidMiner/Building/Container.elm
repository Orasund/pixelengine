module AsteroidMiner.Building.Container exposing (canStore, update)

import AsteroidMiner.Building as Building exposing (BuildingType(..), Volume(..))
import AsteroidMiner.Data exposing (maxValue)
import AsteroidMiner.Data.Item exposing (Item)
import AsteroidMiner.Data.Map exposing (Command, Neighborhood)
import AsteroidMiner.Lib.Command as Command
import AsteroidMiner.Lib.Neighborhood as Neighborhood


canStore : Neighborhood -> Item -> { value : Int, item : Item } -> Bool
canStore _ input { value, item } =
    (input == item) && ((value) < maxValue)


update : Volume -> { value : Int, item : Maybe Item } -> Neighborhood -> Command
update volume { value } neigh =
    let
        transition : Volume -> Command
        transition v =
            if volume == v then
                Command.idle

            else
                Command.transition <| Container v
    in
    neigh
        |> Neighborhood.filter Building.isOutput
        |> List.map (Tuple.first >> Command.send)
        |> (::)
            (if value == 0 then
                transition Empty

             else if value < maxValue // 2 then
                transition HalfEmpty

             else if value == maxValue then
                transition Full

             else
                transition HalfFull
            )
        |> Command.batch
