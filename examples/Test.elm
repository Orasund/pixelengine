module TestExample exposing (main)

import Css
import Html.Styled exposing (toUnstyled)
import PixelEngine.Graphics as Graphics exposing (Background)
import PixelEngine.Graphics.Image exposing (fromTile, image, multipleImages)
import PixelEngine.Graphics.Tile as Tile
    exposing
        ( Tileset
        , tile
        )


main =
    let
        tileSize : Int
        tileSize =
            16

        windowWidth : Int
        windowWidth =
            16

        scale : Float
        scale =
            2

        width : Float
        width =
            (toFloat <| windowWidth * tileSize) * scale

        tileset : Tileset
        tileset =
            { source = "blue_bar.png", spriteWidth = 16, spriteHeight = 4 }

        background : Background
        background =
            Graphics.colorBackground (Css.rgb 20 12 28)
    in
    Graphics.render
        { width = width, transitionSpeedInSec = 0.2, scale = scale }
        [ Graphics.imageArea
            { height = scale * (toFloat <| tileSize * 12)
            , background = background
            }
            [ ( ( 0, 0 )
              , multipleImages
                    [ ( ( 0, 0 ), fromTile (tile ( 0, 0 ) |> Tile.animated 15) tileset )
                    , ( ( 0, 8 * 1 ), fromTile (tile ( 1, 0 ) |> Tile.animated 14) tileset )
                    , ( ( 0, 8 * 2 ), fromTile (tile ( 2, 0 ) |> Tile.animated 13) tileset )
                    , ( ( 0, 8 * 3 ), fromTile (tile ( 3, 0 ) |> Tile.animated 12) tileset )
                    , ( ( 0, 8 * 4 ), fromTile (tile ( 4, 0 ) |> Tile.animated 11) tileset )
                    , ( ( 0, 8 * 5 ), fromTile (tile ( 5, 0 ) |> Tile.animated 10) tileset )
                    , ( ( 0, 8 * 6 ), fromTile (tile ( 6, 0 ) |> Tile.animated 9) tileset )
                    , ( ( 0, 8 * 7 ), fromTile (tile ( 7, 0 ) |> Tile.animated 8) tileset )
                    ]
              )
            ]
        ]
        |> toUnstyled
