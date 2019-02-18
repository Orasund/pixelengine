module PixelEngine.Grid.Position exposing
    ( Coord
    , Position
    , add
    , difference
    , fromDirection
    , move
    , toDirection
    , distance
    , length
    , scaleBy
    )

import PixelEngine.Grid.Direction exposing (Direction(..))


{-| A `Position` is a comparable representation for 2D Coordinates.

You should store your coordinates in this type.

-}
type alias Position =
    ( Int, Int )


{-| `Coord` is a non-comparable representation for 2D Coordinates.

Use this type for your calculations.

-}
type alias Coord =
    { x : Int
    , y : Int
    }


{-| Apply Coordinates to a position to get the relative position.
-}
add : Coord -> Position -> Position
add v ( x, y ) =
    ( x + v.x
    , y + v.y
    )

scaleBy : Int -> Coord -> Coord
scaleBy n {x,y} =
    {x=x*n
    ,y=y*n
    }

{-| Returns the difference between two positions.
```
difference p1 p2 == p1 <------- p2
```
-}
difference : Position -> Position -> Coord
difference ( x1, y1 ) ( x2, y2 ) =
    { x = x1 - x2
    , y = y1 - y2
    }

{-| returns the length of the coordinate (distance to (0,0) -}
length : Coord -> Float
length {x,y} =
    distance (0,0) (x,y)

{-| Gets the distance between to points.
(The length of the difference)
-}
distance : Position -> Position -> Float
distance p1 p2 =
    let
        { x, y } =
            difference p1 p2
    in
    sqrt <| toFloat <| (x * x + y * y)


{-| converts a `Direction` into a `Vector` with length `1`.
-}
fromDirection : Direction -> Coord
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

fromValue (0,0) == Right
fromValue (1,1) == Down
fromValue (-1,1) == Down
fromValue (1,-1) == Up
fromValue (-1,-1) == Up

-}
toDirection : Coord -> Direction
toDirection { x, y } =
    if abs x >= abs y then
        if x >= 0 then
            Right

        else
            Left

    else if y >= 0 then
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
    add <| scaleBy n dir 