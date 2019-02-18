module PixelEngine.Graphics.Options exposing
    ( Options
    , fromWidth
    , withAnimationFPS
    , withScale
    , withMovementSpeed
    )

{-| Options are tweak different aspects of your program.


## Options

@docs Options,fromWidth,withAnimationFPS,withMovementSpeed,withScale

-}

import PixelEngine.Graphics.Data.Options as OptionsData


{-| Options for the render function
-}
type alias Options msg =
    OptionsData.Options msg


{-| Defines the width of the game.
-}
fromWidth : Float -> Options msg
fromWidth width =
    OptionsData.new
        { width = width
        , scale = 1
        , movementSpeedInSec = 0.2
        , animationFPS = 1
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
