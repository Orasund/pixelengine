module Grid.Direction exposing
    ( Direction(..)
    , flip
    , rotLeft
    , rotRight
    )

{-| A abstract concept of a direction on a grid.
-}


type Direction
    = Up
    | Down
    | Left
    | Right


{-| Rotates a `Direction` for 180 Degrees.
-}
flip : Direction -> Direction
flip direction =
    case direction of
        Up ->
            Down

        Down ->
            Up

        Left ->
            Right

        Right ->
            Left


{-| Rotates a `Direction` clockwise
-}
rotLeft : Direction -> Direction
rotLeft direction =
    case direction of
        Up ->
            Left

        Left ->
            Down

        Down ->
            Right

        Right ->
            Up


{-| Rotates a `Direction` counter-clockwise
-}
rotRight : Direction -> Direction
rotRight direction =
    case direction of
        Up ->
            Right

        Right ->
            Down

        Down ->
            Left

        Left ->
            Up
