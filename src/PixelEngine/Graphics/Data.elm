module PixelEngine.Graphics.Data exposing( Background(..), Dimensions,Position,Location)

import Color exposing (Color)

type Background
    = ColorBackground Color
    | ImageBackground { source : String, width : Float, height : Float }

type alias Dimensions =
    { width : Float
    , height : Float
    }

type alias Position =
    { top : Float
    , left : Float
    }

type alias Location =
    ( Int, Int )