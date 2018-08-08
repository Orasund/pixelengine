module PixelEngine.ScreenTransition exposing (Transition, apply, customTransition)

{-| adding Transitions between Screens.

this module replaces Graphics.render with ScreenTransition.apply.

To get started, copy the following example:

    Transition.customTransition
        "death_transition"
        [ ( 0, "opacity:1;filter:grayscale(0%) blur(0px);" )
        , ( 1, "opacity:1;filter:grayscale(70%) blur(0px);" )
        , ( 3, "opacity:0;filter:grayscale(70%) blur(5px);" )
        ]
    |> Transition.apply
        options
        { from = ListOfArea
        , to = AnotherListOfArea
        }


## Main Function

@docs apply


## Area

@docs Transition,customTransition

-}

import Css exposing (px)
import Css.Foreign as Foreign
import Html.Styled exposing (Html, div)
import Html.Styled.Attributes exposing (css)
import PixelEngine.Graphics as Graphics exposing (Area)


{-| a Transition between screens
-}
type Transition
    = Transition { name : String, transition : List ( Float, String ) }


{-| the default constructor for a Transition.

For the future i have planed to make transitions modular, similar to a Msg or a Sub.

    Transition.customTransition
        "death_transition"
        [ ( 0, "opacity:1;filter:grayscale(0%) blur(0px);" )
        , ( 1, "opacity:1;filter:grayscale(70%) blur(0px);" )
        , ( 3, "opacity:0;filter:grayscale(70%) blur(5px);" )
        ]

The first value is the duration of the effect, the second is the Css-command at that point in time.
So the example will compile to something like this:

    dealth_transition:
    0% {opacity:1;filter:grayscale(0%) blur(0px);}
    25% {opacity:1;filter:grayscale(70%) blur(0px);}
    100% {opacity:0;filter:grayscale(70%) blur(5px);}

**Note:**

A screen will be automatically hidden after a transition, so the example would also work without the opacity-parameter.

-}
customTransition : String -> List ( Float, String ) -> Transition
customTransition name transition =
    Transition { name = name, transition = transition }


{-| applys a transition to two screen.

It has the same options as Graphics.render, so this is a more advanced version of render.

    apply options {from: ListOfAreas,to:ListOfAreas} {name: "",[(0,"")]}
    =
    render options ListOfAreas

-}
apply :
    { width : Float, scale : Float, transitionSpeedInSec : Float }
    -> { from : List (Area msg), to : List (Area msg) }
    -> Transition
    -> Html msg
apply ({ width } as options) { from, to } (Transition { name, transition }) =
    let
        transitionLength : Float
        transitionLength =
            transition
                |> List.map Tuple.first
                |> List.sum

        animationCss =
            transition
                |> List.foldl
                    (\( length, cssCommands ) ( sum, string ) ->
                        ( sum + length
                        , string
                            ++ (toString <| (sum + length) * 100 / transitionLength)
                            ++ "% {"
                            ++ "visibility:visible;"
                            ++ cssCommands
                            ++ "} "
                        )
                    )
                    ( 0, "" )
                |> Tuple.second
                |> (\a -> a ++ ";")
    in
    div
        [ css
            [ Css.backgroundColor (Css.rgb 0 0 0)
            ]
        ]
        [ Foreign.global
            [ Foreign.selector
                ("@keyframes pixelengine_screen_transition_"
                    ++ name
                )
                [ Css.property
                    animationCss
                    ""
                ]
            ]
        , div
            [ css
                [ Css.position Css.relative
                , Css.width <| px <| width
                , Css.margin Css.auto
                ]
            ]
            [ div
                []
                [ Graphics.render options to ]
            , div
                [ css
                    [ Css.position Css.absolute
                    , Css.top <| px 0
                    , Css.left <| px 0
                    , Css.visibility Css.hidden
                    , Css.property "animation"
                        ("pixelengine_screen_transition_"
                            ++ name
                            ++ " "
                            ++ toString transitionLength
                            ++ "s 1"
                        )
                    ]
                ]
                [ Graphics.render options from ]
            ]
        ]
