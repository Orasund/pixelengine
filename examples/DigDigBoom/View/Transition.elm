module DigDigBoom.View.Transition exposing (nextLevel,death )

import PixelEngine.Graphics.Transition as Transition exposing (Transition)
import PixelEngine.Graphics as Graphics exposing (Area)
import PixelEngine.Graphics.Options as Options exposing (Options)

nextLevel : List (Area msg) -> (Options msg -> Options msg)
nextLevel list =
    Transition.from list <|
      Transition.custom
          "next_level"
          [ ( 0, "filter:saturate(200%) contrast(100%);overflow:hidden;width:100%" )
          , ( 2, "filter:saturate(50%) contrast(150%);overflow:hidden;width:0%;" )
          ]


death : List (Area msg) -> (Options msg -> Options msg)
death list =
  Transition.from list <|
    Transition.custom
        "death_transition"
        [ ( 0, "opacity:1;filter:grayscale(0%) blur(0px);" )
        , ( 1, "opacity:1;filter:grayscale(70%) blur(0px);" )
        , ( 3, "opacity:0;filter:grayscale(70%) blur(5px);" )
        ]