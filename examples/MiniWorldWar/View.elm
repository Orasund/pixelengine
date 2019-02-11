module MiniWorldWar.View exposing (tileSize,continentToPosition)

import MiniWorldWar.Data.Continent as Continent exposing (Continent(..))

tileSize : Float
tileSize =
    16

continentToPosition : Continent -> ( Float, Float )
continentToPosition continent =
    case continent of
        Europe ->
            ( tileSize * 1, tileSize * 1 )

        NorthAmerica ->
            ( tileSize * 5, tileSize * 1 )

        SouthAmerica ->
            ( tileSize * 5, tileSize * 4 )

        Asia ->
            ( tileSize * 3, tileSize * 1 )

        Africa ->
            ( tileSize * 1, tileSize * 4 )