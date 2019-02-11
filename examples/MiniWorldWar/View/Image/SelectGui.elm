module MiniWorldWar.View.Image.SelectGui exposing
    ( SelectGui
    , addUnitButton
    , cardButton
    , centerCardButton
    , removeUnitButton
    , selectGui
    , supply
    , swapUnitsButton
    )

import PixelEngine.Graphics.Image as Image exposing (Image, image)
import PixelEngine.Graphics.Tile exposing (Tile, Tileset, animated, tile)


type alias SelectGui =
    { selected : Int, remaining : Int }


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
    tile ( boundedN, 0 )


supply : Image msg
supply =
    Image.fromTile (tile ( 5, 1 )) tileset


removeUnitButton : Image msg
removeUnitButton =
    Image.fromTile (tile ( 3, 1 )) tileset


addUnitButton : Image msg
addUnitButton =
    Image.fromTile (tile ( 2, 1 )) tileset


centerCardButton : Image msg
centerCardButton =
    Image.fromTile (tile ( 1, 1 )) tileset


cardButton : Image msg
cardButton =
    Image.fromTile (tile ( 0, 1 )) tileset


swapUnitsButton : Image msg
swapUnitsButton =
    Image.fromTile (tile ( 4, 1 )) tileset


selectGui : SelectGui -> Image msg
selectGui { selected, remaining } =
    Image.multipleImages
        [ ( ( 0, 0 ), image "bigMarker.png" )
        , ( ( 0, 3 + 0 ), Image.fromTile (number remaining) tileset )
        , ( ( 8, 3 + 0 ), Image.fromTile (tile ( 11, 0 )) tileset )
        , ( ( 8 * 2, 3 + 0 ), Image.fromTile (number selected) tileset )
        ]
