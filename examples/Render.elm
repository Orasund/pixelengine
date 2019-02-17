module RenderExample exposing (main)

import Color
import Html
import Html.Attributes as Attributes
import PixelEngine.Graphics as Graphics exposing (Background)
import PixelEngine.Graphics.Image as Image exposing (image)
import PixelEngine.Graphics.Tile exposing (Tileset)


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

        font : Tileset
        font =
            { source = "https://orasund.github.io/pixelengine/fonts/RetroDeco8x16.png"
            , spriteWidth = 8
            , spriteHeight = 16
            }

        background : Background
        background =
            Graphics.colorBackground (Color.rgb255 222 238 214)
    in
    Html.div []
        [ Html.text "Games can be included in websides."
        , Html.div [ Attributes.style "width" <| (String.fromFloat <| width) ++ "px" ]
            [ Graphics.render
                2
                (Graphics.options
                    { width = width
                    , transitionSpeedInSec = 0.2
                    }
                )
                [ Graphics.imageArea
                    { height = 64
                    , background = background
                    }
                    [ ( ( 32, 0 )
                      , image "https://orasund.github.io/pixelengine/pixelengine-logo.png"
                      )
                    , ( ( width / 2 + 16, 8 ), Image.fromTextWithSpacing -1 "powered by" font )
                    , ( ( width / 2, 32 ), Image.fromText "PixelEngine" font )
                    ]
                ]
            ]
        , Html.text "Use a div and set a width using CSS, to remove the black sidebars of the game."
        ]
