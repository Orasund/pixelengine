module DigDigBoom.View.Transition exposing (death, nextLevel)

import PixelEngine exposing (Area)
import PixelEngine.Options as Options exposing (Options)


nextLevel : List (Area msg) -> (Options msg -> Options msg)
nextLevel list =
    Options.withTransitionFrom list <|
        Options.transition
            "next_level"
            { start = "filter:saturate(200%) contrast(100%);overflow:hidden;width:100%"
            , keyFrames = [ Nothing ]
            , end = "filter:saturate(50%) contrast(150%);overflow:hidden;width:0%;"
            }


death : List (Area msg) -> (Options msg -> Options msg)
death list =
    Options.withTransitionFrom list <|
        Options.transition
            "death_transition"
            { start = "opacity:1;filter:grayscale(0%) blur(0px);"
            , keyFrames =
                [ Just "opacity:1;filter:grayscale(70%) blur(0px);"
                , Nothing
                ]
            , end = "opacity:0;filter:grayscale(70%) blur(5px);"
            }
