module TilesetExample exposing (main)

import Css exposing (px)
import Html.Styled exposing (toUnstyled)
import PixelEngine.Graphics as Graphics exposing (Background(..), Tileset)


main =
    let
        tileSize : Int
        tileSize =
            16

        windowSize : Int
        windowSize =
            16

        scale : Int
        scale =
            2

        width : Css.Px
        width =
            px <| toFloat <| (windowSize * tileSize * scale)

        tileset : Tileset
        tileset =
            { source = "tileset.png", width = 16, height = 16 }

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
        { scale = scale, width = width }
        [ Graphics.tiledArea { height = windowSize, tileset = tileset, background = Color (Css.rgb 20 12 28) }
            [ ( ( 6, 7 ), goblin )
            , ( ( 7, 7 ), letter_h )
            , ( ( 8, 7 ), letter_i )
            , ( ( 9, 7 ), heart )
            ]
        ]
        |> toUnstyled
