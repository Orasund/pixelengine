module Index.Home exposing (view)

import Color
import Element exposing (Element)
import Element.Background as Background
import Framework.Button as Button
import Framework.Card as Card
import Framework.Modifier exposing (Modifier(..))
import Framework.Typography as Typography
import PixelEngine.Graphics as Graphics exposing (Background)
import PixelEngine.Graphics.Image as Image exposing (image)
import PixelEngine.Graphics.Tile exposing (Tileset)


card : String -> Element msg
card name =
    Card.simple <|
        Element.column
            [ Element.width <| Element.px 350
            , Element.height <| Element.px 400
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
                    ("https://github.com/Orasund/pixelengine/blob/master/examples/" ++ name ++ "/Main.elm")
                    "Source"
                ]
            , Element.link []
                { url = "#" ++ name
                , label =
                    Element.image [ Element.width <| Element.fill ]
                        { src = "examples/" ++ name ++ "/" ++ "preview.png"
                        , description = "Preview"
                        }
                }
            ]


view : { examples : List String, games : List String } -> Element msg
view { examples, games } =
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
        , Element.column [ Element.spacing 10, Element.centerX ]
            [ Typography.h1 [ Element.centerX ] <| Element.text "Examples"
            , Element.wrappedRow [ Element.centerX, Element.spacing 10 ]
                (examples |> List.map card)
            ]
        , Element.column [ Element.spacing 10, Element.centerX ]
            [ Typography.h1 [ Element.centerX ] <| Element.text "Games"
            , Element.wrappedRow [ Element.centerX, Element.spacing 10 ]
                (games |> List.map card)
            ]
        ]
