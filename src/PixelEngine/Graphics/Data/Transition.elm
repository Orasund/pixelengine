module PixelEngine.Graphics.Data.Transition exposing (Transition(..))

type Transition
    = Transition { name : String, transitionList : List ( Float, String ) }