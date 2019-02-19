module MiniWorldWar.View.Image.SelectGui exposing
    ( addUnitButton
    , cardButton
    , centerCardButton
    , removeUnitButton
    , selectGui
    , supply
    , swapUnitsButton
    )

import PixelEngine.Image as Image exposing (Image)
import PixelEngine.Tile as Tile exposing (Tile, Tileset)


tileset : Tileset
tileset =
    { source = "guiSymbols.png"
    , spriteWidth = 8
    , spriteHeight = 8
    }


number : Int -> Tile msg
number n =
    let
        boundedN : Int
        boundedN =
            if n > 10 then
                10

            else
                n
    in
    Tile.fromPosition ( boundedN, 0 )


supply : Image msg
supply =
    Image.fromTile (Tile.fromPosition ( 5, 1 )) tileset


removeUnitButton : Image msg
removeUnitButton =
    Image.fromTile (Tile.fromPosition ( 3, 1 )) tileset


addUnitButton : Image msg
addUnitButton =
    Image.fromTile (Tile.fromPosition ( 2, 1 )) tileset


centerCardButton : Image msg
centerCardButton =
    Image.fromTile (Tile.fromPosition ( 1, 1 )) tileset


cardButton : Image msg
cardButton =
    Image.fromTile (Tile.fromPosition ( 0, 1 )) tileset


swapUnitsButton : Image msg
swapUnitsButton =
    Image.fromTile (Tile.fromPosition ( 4, 1 )) tileset


selectGui : { selected : Int, remaining : Int } -> Image msg
selectGui { selected, remaining } =
    Image.multipleImages
        [ ( ( 0, 0 ), Image.fromSrc "bigMarker.png" )
        , ( ( 0, 3 + 0 ), Image.fromTile (number remaining) tileset )
        , ( ( 8, 3 + 0 ), Image.fromTile (Tile.fromPosition ( 11, 0 )) tileset )
        , ( ( 8 * 2, 3 + 0 ), Image.fromTile (number selected) tileset )
        ]
