module TestExample exposing (main)

import Color
import PixelEngine.Graphics as Graphics exposing (Background)
import PixelEngine.Graphics.Image exposing (image)
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

        width : Float
        width =
            toFloat <| windowWidth * tileSize

        tileset : Tileset
        tileset =
            { source = "https://orasund.github.io/pixelengine/DigDigBoom/tileset.png"
            , spriteWidth = tileSize
            , spriteHeight = tileSize
            }

        background : Background
        background =
            Graphics.colorBackground (Color.rgb255 20 12 28)

        goblin =
            tile ( 2, 8 ) |> Tile.animated 1

        letter_h =
            tile ( 1, 15 )

        letter_i =
            tile ( 2, 12 )

        heart =
            tile ( 4, 8 )
    in
    Graphics.render
        2
        (Graphics.options
            { width = width
            , transitionSpeedInSec = 0.2
            }
        )
        [ Graphics.tiledArea
            { rows = 4
            , tileset = tileset
            , background = background
            }
            [ ( ( 6, 2 ), goblin |> Tile.withAttributes [ Tile.backgroundColor (Color.rgb255 170 57 57) ] )
            , ( ( 7, 2 ), letter_h |> Tile.withAttributes [ Tile.backgroundColor (Color.rgb255 97 81 146) ] )
            , ( ( 8, 2 ), letter_i |> Tile.withAttributes [ Tile.backgroundColor (Color.rgb255 170 151 57) ] )
            , ( ( 9, 2 ), heart |> Tile.withAttributes [ Tile.backgroundColor (Color.rgb255 45 134 51) ] )
            ]
        , Graphics.imageArea
            { height = toFloat <| tileSize * 12
            , background = background
            }
            [ ( ( width / 2 - 80, 0 )
              , image "https://orasund.github.io/pixelengine/pixelengine-logo.png"
              )
            ]
        ]
