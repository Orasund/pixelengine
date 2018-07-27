module TilesetExample exposing (main)

import Css exposing (px)
import Html.Styled exposing (toUnstyled)
import PixelEngine.Graphics as Graphics exposing (Background(..), SupportedUnit(..), Tileset)


main =
    let
        tileSize : Int
        tileSize =
            16

        windowSize : Int
        windowSize =
            16

        scale : Float
        scale =
            2

        width : Float
        width =
            (toFloat <| windowSize * tileSize) * scale

        tileset : Tileset
        tileset =
            { source = "tileset.png", spriteWidth = tileSize, spriteHeight = tileSize }

        background : Background
        background =
            Image { src = "background.png", width = 16 * scale, height = 16 * scale }

        --Color (Css.rgb 20 12 28)
        goblin =
            Graphics.animatedTile ( 2, 8 ) 1

        letter_h =
            Graphics.tile ( 1, 15 )

        letter_i =
            Graphics.tile ( 2, 12 )

        heart =
            Graphics.tile ( 4, 8 )
    in
    Graphics.render
        { width = width, unit = Px }
        [ Graphics.tiledArea
            { rows = windowSize
            , cols = windowSize
            , tileset = tileset
            , background = background
            }
            [ ( ( 6, 7 ), goblin )
            , ( ( 7, 7 ), letter_h )
            , ( ( 8, 7 ), letter_i )
            , ( ( 9, 7 ), heart )
            ]
        ]
        |> toUnstyled
