module MiniWorldWar.Data.Continent exposing (Continent(..), list, neighbour, toInt)

import MiniWorldWar.Data.Direction exposing (Direction(..))


list : List Continent
list =
    [ Europe, Asia, NorthAmerica, SouthAmerica, Africa ]


type Continent
    = Europe
    | Asia
    | NorthAmerica
    | SouthAmerica
    | Africa


toInt : Continent -> Int
toInt continent =
    case continent of
        SouthAmerica ->
            0

        NorthAmerica ->
            1

        Asia ->
            2

        Europe ->
            3

        Africa ->
            4


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
