module PixelEngine.Graphics.Data.Options exposing (Options(..),new,usingScale)

import PixelEngine.Graphics.Data.Controller exposing (ControllerOptions)
import PixelEngine.Graphics.Data.Area exposing (Area)
import PixelEngine.Graphics.Data.Transition exposing (Transition(..))

usingScale : Float -> Options msg -> Options msg
usingScale scale (Options o) =
    Options { o | scale = scale }

type Options msg
    = Options
        { width : Float
        , scale : Float
        , transitionSpeedInSec : Float
        , controllerOptions : Maybe (ControllerOptions msg)
        , transitionFrom : List (Area msg)
        , transition : Transition
        }


new : { width : Float, scale : Float, transitionSpeedInSec : Float } -> Options msg
new { width, scale, transitionSpeedInSec } =
    Options
        { width = width
        , scale = scale
        , transitionSpeedInSec = transitionSpeedInSec
        , controllerOptions = Nothing
        , transitionFrom = []
        , transition = Transition { name = "", transitionList = [ ( 0, "" ) ] }
        }