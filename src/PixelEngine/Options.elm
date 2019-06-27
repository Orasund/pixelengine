module PixelEngine.Options exposing
    ( withTransitionFrom
    , Transition, transition
    , Options, default, withAnimationFPS, withMovementSpeed, withScale
    )

{-| Options tweak different aspects of your program.

#Transitions
Adding a transitions between screens.

The information for the transition will be written in the `Option` class from
[PixelEngine.Graphics](PixelEngine-Graphics).

To get started, copy the following example:

    options
        |> Transition.withTransitionFrom
            ListOfArea
            (Transition.customTransition
                "death_transition"
                [ ( 0, "opacity:1;filter:grayscale(0%) blur(0px);" )
                , ( 1, "opacity:1;filter:grayscale(70%) blur(0px);" )
                , ( 3, "opacity:0;filter:grayscale(70%) blur(5px);" )
                ]
            )


## Main Function

@docs withTransitionFrom


## Area

@docs Transition, transition


## Options

@docs Options, default, withAnimationFPS, withMovementSpeed, withScale

-}

import PixelEngine.Graphics.Data.Area exposing (Area)
import PixelEngine.Graphics.Data.Options as OptionsData
import PixelEngine.Graphics.Data.Transition as TransitionData



----------------------------------------
-- Transition
----------------------------------------


{-| A transition between screens
-}
type alias Transition =
    TransitionData.Transition


{-| The default constructor for a `Transition`.

For the future I have planed to make transitions modular, similar to a `Msg` or a `Sub`.

    Transition.transition
        "death_transition"
        [ ( 0, "opacity:1;filter:grayscale(0%) blur(0px);" )
        , ( 1, "opacity:1;filter:grayscale(70%) blur(0px);" )
        , ( 3, "opacity:0;filter:grayscale(70%) blur(5px);" )
        ]

The first value is the duration of the effect, the second is the CSS-command at that point in time.
So the example will compile to something like this:

    dealth_transition:
    0% {opacity:1;filter:grayscale(0%) blur(0px);}
    25% {opacity:1;filter:grayscale(70%) blur(0px);}
    100% {opacity:0;filter:grayscale(70%) blur(5px);}

**Note:**
A screen will be automatically hidden after a transition,
so the example would also work without the opacity-parameter.

-}
transition : String -> { start : String, keyFrames : List (Maybe String), end : String } -> Transition
transition name { start, keyFrames, end } =
    let
        transitionList : List ( Float, String )
        transitionList =
            ( 0, start )
                :: (keyFrames
                        |> List.foldr
                            (\maybeString ( list, i ) ->
                                ( case maybeString of
                                    Just string ->
                                        ( toFloat <| i, string ) :: list

                                    Nothing ->
                                        list
                                , i - 1
                                )
                            )
                            ( [ ( toFloat <| (keyFrames |> List.length) + 1, end ) ], keyFrames |> List.length )
                        |> Tuple.first
                   )
    in
    TransitionData.Transition { name = name, transitionList = transitionList }



----------------------------------------
-- Options
----------------------------------------


{-| Options for the render function
-}
type alias Options msg =
    OptionsData.Options msg


{-| Defines the width of the game.
-}
default : Options msg
default =
    OptionsData.new


{-| adds the `Transition` to the `Options`.

The first argument is the List or Areas taken **before** the transition is applied.
(e.g. the last state)

-}
withTransitionFrom : List (Area msg) -> Transition -> Options msg -> Options msg
withTransitionFrom listOfArea t (OptionsData.Options options) =
    OptionsData.Options
        { options
            | transitionFrom = listOfArea
            , transition = t
        }


{-| Sets the Frames per Seconds for Animations.

Value must be positv or else the function will be ignored.

**Default value:** `1`

-}
withAnimationFPS : Float -> Options msg -> Options msg
withAnimationFPS =
    OptionsData.withAnimationFPS


{-| The speed of movement in seconds.

**Default value:** `0.2`

-}
withMovementSpeed : Float -> Options msg -> Options msg
withMovementSpeed =
    OptionsData.withMovementSpeed


{-| Scales up everything.

Use only power of `2` as scale to ensure crisp pixels.

**Default value:** `1`

-}
withScale : Int -> Options msg -> Options msg
withScale =
    OptionsData.usingScale
