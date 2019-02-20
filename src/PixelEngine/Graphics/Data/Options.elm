module PixelEngine.Graphics.Data.Options exposing
    ( Options(..)
    , new
    , usingScale
    , withAnimationFPS
    , withMovementSpeed
    , withWidth
    )

import PixelEngine.Graphics.Data.Area exposing (Area)
import PixelEngine.Graphics.Data.Controller exposing (ControllerOptions)
import PixelEngine.Graphics.Data.Transition exposing (Transition(..))


type Options msg
    = Options
        { width : Float
        , scale : Int
        , movementSpeedInSec : Float
        , animationFPS : Float
        , controllerOptions : Maybe (ControllerOptions msg)
        , transitionFrom : List (Area msg)
        , transition : Transition
        }


new : Options msg
new =
    Options
        { width = 512
        , scale = 1
        , movementSpeedInSec = 0.2
        , animationFPS = 1
        , controllerOptions = Nothing
        , transitionFrom = []
        , transition = Transition { name = "", transitionList = [ ( 0, "" ) ] }
        }


withWidth : Float -> Options msg -> Options msg
withWidth width (Options o) =
    Options { o | width = width }


withAnimationFPS : Float -> Options msg -> Options msg
withAnimationFPS fps (Options o) =
    if fps > 0 then
        Options { o | animationFPS = fps }

    else
        Options o


withMovementSpeed : Float -> Options msg -> Options msg
withMovementSpeed movementSpeed (Options o) =
    Options { o | movementSpeedInSec = movementSpeed }


usingScale : Int -> Options msg -> Options msg
usingScale scale (Options o) =
    Options { o | scale = scale }
