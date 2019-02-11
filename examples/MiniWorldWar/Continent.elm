module MiniWorldWar.Continent exposing (Continent(..),list,neighbour)

import MiniWorldWar.Direction as Direction exposing (Direction(..))

list : List Continent
list = [Europe,Asia,NorthAmerica,SouthAmerica,Africa]

type Continent =
    Europe
    | Asia
    | NorthAmerica
    | SouthAmerica
    | Africa

neighbour : Direction -> Continent -> Continent
neighbour direction continent =
    case ( continent, direction ) of
        ( Europe, Left ) ->
            NorthAmerica

        ( Europe, Right ) ->
            Asia

        ( Europe, Down ) ->
            Africa

        ( Asia, Left ) ->
            Europe

        ( Asia, Right ) ->
            NorthAmerica

        ( NorthAmerica, Left ) ->
            Asia

        ( NorthAmerica, Right ) ->
            Europe

        ( NorthAmerica, Down ) ->
            SouthAmerica

        ( Africa, Up ) ->
            Europe

        ( Africa, Left ) ->
            SouthAmerica

        ( SouthAmerica, Up ) ->
            NorthAmerica

        ( SouthAmerica, Right ) ->
            Africa

        _ ->
            Asia