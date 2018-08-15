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
            { source = "blue_bar.png", spriteWidth = 16, spriteHeight = 4}

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
                    [ ( ( 0, 0 ), fromTile ( tile (13,0)|> Tile.animated 2) tileset )
                    ]
              )
            ]
        ]
        |> toUnstyled