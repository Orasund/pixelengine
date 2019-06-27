module AsteroidMiner.View.Tileset exposing
    ( background
    , coloredConveyorBelt
    , comet
    , container
    , conveyorBelt
    , font
    , ground
    , itemBackground
    , merger
    , mine
    , mountain
    , oreGround
    , sorter
    , stone
    , tileset
    , valid
    )

import AsteroidMiner.Building exposing (BeltColor(..), Code(..), Volume(..))
import Grid.Direction exposing (Direction(..))
import Grid.Position as Position exposing (Coord, Position)
import PixelEngine.Tile as Tile exposing (Tile, Tileset)


font : Tileset
font =
    Tile.tileset
        { source = "Expire8x8.png"
        , spriteWidth = 8
        , spriteHeight = 8
        }


tileset : Tileset
tileset =
    Tile.tileset
        { source = "tileset.png"
        , spriteWidth = 8
        , spriteHeight = 8
        }


background : Tile msg
background =
    Tile.fromPosition ( 0, 0 )


valid : Tile msg
valid =
    Tile.fromPosition ( 4, 5 )
        |> Tile.animated 4


ground : Tile msg
ground =
    Tile.fromPosition ( 1, 0 )


mountain : Tile msg
mountain =
    Tile.fromPosition ( 0, 1 )


oreGround : Tile msg
oreGround =
    Tile.fromPosition ( 1, 1 )


comet : Tile msg
comet =
    Tile.fromPosition ( 4, 4 )
        |> Tile.animated 4
        |> Tile.movable "comet"


mine : Tile msg
mine =
    Tile.fromPosition ( 4, 2 )
        |> Tile.animated 4


conveyorBelt : Code -> Tile msg
conveyorBelt code =
    case code of
        Invalid ->
            Tile.fromPosition ( 0, 2 )

        InputFound ->
            Tile.fromPosition ( 1, 2 )

        Try Blue ->
            Tile.fromPosition ( 4, 6 )

        Try Green ->
            Tile.fromPosition ( 5, 6 )

        Try Red ->
            Tile.fromPosition ( 6, 6 )

        Try Yellow ->
            Tile.fromPosition ( 7, 6 )

        Failed Blue ->
            Tile.fromPosition ( 4, 7 )

        Failed Green ->
            Tile.fromPosition ( 5, 7 )

        Failed Red ->
            Tile.fromPosition ( 6, 7 )

        Failed Yellow ->
            Tile.fromPosition ( 7, 7 )


coloredConveyorBelt : BeltColor -> Direction -> Tile msg
coloredConveyorBelt color dir =
    let
        getCoords : Coord
        getCoords =
            case dir of
                Right ->
                    { x = 0, y = 0 }

                Down ->
                    { x = 1, y = 0 }

                Up ->
                    { x = 2, y = 0 }

                Left ->
                    { x = 3, y = 0 }

        pos : Position
        pos =
            case color of
                Blue ->
                    ( 0, 4 )

                Green ->
                    ( 0, 5 )

                Red ->
                    ( 0, 6 )

                Yellow ->
                    ( 0, 7 )
    in
    pos
        |> Position.add getCoords
        |> Tile.fromPosition


container : Volume -> Tile msg
container volume =
    case volume of
        Empty ->
            Tile.fromPosition ( 2, 0 )

        HalfEmpty ->
            Tile.fromPosition ( 3, 0 )

        HalfFull ->
            Tile.fromPosition ( 2, 1 )

        Full ->
            Tile.fromPosition ( 3, 1 )


merger : Tile msg
merger =
    Tile.fromPosition ( 4, 0 )
        |> Tile.animated 4


sorter : Tile msg
sorter =
    Tile.fromPosition ( 4, 1 )
        |> Tile.animated 4


itemBackground : Tile msg
itemBackground =
    Tile.fromPosition ( 2, 2 )


stone : Tile msg
stone =
    Tile.fromPosition ( 3, 2 )
