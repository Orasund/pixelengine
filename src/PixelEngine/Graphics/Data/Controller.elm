module PixelEngine.Graphics.Data.Controller exposing (AbstractInput(..), ControllerOptions, render)

import Css exposing (px)
import Html.Styled as Html exposing (Html, div)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events as Events
import PixelEngine.Graphics.Data exposing (Dimensions)


type AbstractInput
    = AbstractInputLeft
    | AbstractInputRight
    | AbstractInputUp
    | AbstractInputDown
    | AbstractInputA
    | AbstractInputB
    | AbstractInputX
    | AbstractInputY


type alias ControllerOptions msg =
    { windowSize : Dimensions
    , controls : AbstractInput -> Maybe msg
    }


render : ControllerOptions msg -> List (Html msg)
render { windowSize, controls } =
    let
        diameter : Float
        diameter =
            windowSize.height / 4

        circle : String -> AbstractInput -> Float -> List Css.Style -> Html msg
        circle char input size listOfCss =
            Html.button
                (List.append
                    [ css
                        ([ Css.width <| Css.px <| size
                         , Css.height <| Css.px <| size
                         , Css.borderRadius <| Css.px <| size / 2
                         , Css.borderStyle <| Css.none
                         , Css.backgroundColor <| Css.rgb 256 256 256
                         , Css.textAlign Css.center
                         , Css.fontFamily Css.sansSerif
                         , Css.fontSize <| Css.px <| size * 0.9
                         , Css.opacity <| Css.num 0.5
                         ]
                            |> List.append listOfCss
                        )
                    ]
                    (case controls <| input of
                        Just msg ->
                            [ Events.onClick msg ]

                        Nothing ->
                            []
                    )
                )
                [ Html.text char
                ]
    in
    [ div
        [ css
            [ Css.position Css.absolute
            , Css.top <| px 0
            , Css.left <| px 0
            ]
        ]
        [ circle "◀"
            AbstractInputLeft
            diameter
            [ Css.position Css.absolute
            , Css.top <| px <| diameter * 1.5
            , Css.left <| px 0
            ]
        , circle "▶"
            AbstractInputRight
            diameter
            [ Css.position Css.absolute
            , Css.top <| px <| diameter * 1.5
            , Css.left <| px <| diameter + (sqrt (2 * diameter ^ 2) - diameter)
            ]
        , circle "▲"
            AbstractInputUp
            diameter
            [ Css.position Css.absolute
            , Css.top <| px <| diameter * 1.0 - (sqrt (2 * diameter ^ 2) - diameter) / 2
            , Css.left <| px <| diameter * 0.5 + (sqrt (2 * diameter ^ 2) - diameter) / 2
            ]
        , circle "▼"
            AbstractInputDown
            diameter
            [ Css.position Css.absolute
            , Css.top <| px <| diameter * 2.0 + (sqrt (2 * diameter ^ 2) - diameter) / 2
            , Css.left <| px <| diameter * 0.5 + (sqrt (2 * diameter ^ 2) - diameter) / 2
            ]
        ]
    , div
        [ css
            [ Css.position Css.absolute
            , Css.top <| px 0
            , Css.right <| px 0
            ]
        ]
        [ circle "A"
            AbstractInputA
            diameter
            [ Css.position Css.absolute
            , Css.top <| px <| diameter * 1.5
            , Css.right <| px 0
            ]
        , circle "X"
            AbstractInputX
            diameter
            [ Css.position Css.absolute
            , Css.top <| px <| diameter * 1.5
            , Css.right <| px <| diameter + (sqrt (2 * diameter ^ 2) - diameter)
            ]
        , circle "Y"
            AbstractInputY
            diameter
            [ Css.position Css.absolute
            , Css.top <| px <| diameter * 1.0 - (sqrt (2 * diameter ^ 2) - diameter) / 2
            , Css.right <| px <| diameter * 0.5 + (sqrt (2 * diameter ^ 2) - diameter) / 2
            ]
        , circle "B"
            AbstractInputB
            diameter
            [ Css.position Css.absolute
            , Css.top <| px <| diameter * 2.0 + (sqrt (2 * diameter ^ 2) - diameter) / 2
            , Css.right <| px <| diameter * 0.5 + (sqrt (2 * diameter ^ 2) - diameter) / 2
            ]
        ]
    ]
