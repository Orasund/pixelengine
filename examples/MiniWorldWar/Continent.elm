module MiniWorldWar.Continent exposing (Continent(..),list)

list : List Continent
list = [Europe,Asia,NorthAmerica,SouthAmerica,Africa]

type Continent =
    Europe
    | Asia
    | NorthAmerica
    | SouthAmerica
    | Africa

