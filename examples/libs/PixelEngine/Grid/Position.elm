module PixelEngine.Grid.Position exposing
    ( Position
    , Vector
    , difference
    , move
    , add
    , fromDirection
    , toDirection
    )

import PixelEngine.Grid.Direction exposing (Direction(..))

{-| A `Position` is a comparable representation for 2D Coordinates.

You should store your coordinates in this type.
-}
type alias Position = (Int,Int)

{-| A `Vector` is a non-comparable representation for 2D Coordinates.

Use this type for your calculations.
-}
type alias Vector =
    { x : Int
    , y : Int
    }

{-| Returns the Position relative to a Vector
-}
add : Vector -> Position -> Position
add v (x,y) =
    ( x + v.x
    , y + v.y
    )


difference : Position -> Position -> Vector
difference (x1,y1) (x2,y2)=
    { x = x1 - x2
    , y = y1 - y2
    }


{-| converts a `Direction` into a `Vector` with length `1`.
-}
fromDirection : Direction -> Vector
fromDirection direction =
    case direction of
        Up ->
            { x = 0, y = -1 }

        Down ->
            { x = 0, y = 1 }

        Left ->
            { x = -1, y = 0 }

        Right ->
            { x = 1, y = 0 }


{-| converts **any** `Vector` into its nearest `Direction` value.

fromValue (0,0) == Up
fromValue (1,1) == Down
fromValue (-1,1) == Down
fromValue (1,-1) == Up
fromValue (-1,-1) == Up

-}
toDirection : Vector -> Direction
toDirection { x, y } =
    if abs x > abs y then
        if x > 0 then
            Right

        else
            Left

    else if y > 0 then
        Down

    else
        Up


{-| moves a Point some amount of steps in a direction.
-}
move : Int -> Direction -> Position -> Position
move n direction =
    let
        dir =
            direction |> fromDirection
    in
    add
        { x = dir.x * n
        , y = dir.y * n
        }
