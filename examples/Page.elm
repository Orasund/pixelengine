module Page exposing (main)

import Color
import Element exposing (Element)
import Element.Background as Background
import Framework.Button as Button
import Framework.Card as Card
import Framework.Modifier exposing (Modifier(..))
import Framework.Typography as Typography
import Html
import Html.Attributes as Attributes
import PixelEngine.Graphics as Graphics exposing (Background)
import PixelEngine.Graphics.Image as Image exposing (image, multipleImages)
import PixelEngine.Graphics.Tile as Tile exposing (Tileset)


card : String -> Element ()
card name =
    Card.simple <|
        Element.column
            [ Element.width <| Element.px 350
            , Element.height <| Element.px 450
            , Element.spacing 20
            , Element.centerX
            ]
        <|
            [ Element.row
                [ Element.width <| Element.fill
                , Element.spaceEvenly
                ]
                [ Element.text name
                , Button.buttonLink
                    []
                    ("https://github.com/Orasund/pixelengine/blob/master/examples/"++name++"/Main.elm")
                    "Source"
                ]
            , Element.image [ Element.width <| Element.fill ]
                { src = name ++ "/" ++ "preview.png"
                , description = "Preview"
                }
            , Element.row
                [ Element.width <| Element.fill
                , Element.spaceEvenly
                ]
                [ Button.buttonLinkWidth
                    [ Success ]
                    name
                    "Play"
                    200
                ]
            ]


main =
    let
        windowWidth : Int
        windowWidth =
            200

        width : Float
        width =
            toFloat <| windowWidth

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
    Element.layout
        [ Background.color <| Element.rgb255 222 238 214 ]
    <|
        Element.column
            [ Element.spacing 50
            , Element.centerX
            ]
            [ Element.el
                [ Element.height <| Element.px <| 64 * 4
                , Element.centerX
                ]
              <|
                Element.html <|
                    Graphics.render
                        4
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
                            , ( ( width / 2, 8 ), Image.fromTextWithSpacing -2 "Create Games" font )
                            , ( ( width / 2, 32 ), Image.fromTextWithSpacing -3 "with Elm" font )
                            ]
                        ]
            , Element.column [Element.spacing 10, Element.centerX]
                [ Typography.h1 [Element.centerX ] <| Element.text "Examples"
                , Element.wrappedRow [ Element.centerX, Element.spacing 10 ]
                    ([ "Animations", "TicTacToe","Snake"]
                        |> List.map card
                    )
                ]
            , Element.column [Element.spacing 10, Element.centerX]
                [ Typography.h1 [Element.centerX ] <| Element.text "Finished Games"
                , Element.wrappedRow [ Element.centerX, Element.spacing 10 ]
                    ([ "DigDigBoom", "MiniWorldWar", "RuinJump", "CultSim" ]
                        |> List.map card
                    )
                ]
            ]
