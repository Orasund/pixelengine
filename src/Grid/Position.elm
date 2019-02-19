module Grid.Position exposing
    ( Position, move, add, coordsTo
    , Coord, fromDirection, toDirection, distance, scaleBy, length
    )

{-| This module contains two ways of representing a point on a grid:
`Position` and `Coord`. Positions are meant to be stored, and coordinates are
meant to to calculations with.


# Position

@docs Position, move, add, coordsTo

#Vector

@docs Coord, fromDirection, toDirection, distance, scaleBy, length

-}

import Grid.Direction exposing (Direction(..))


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


{-| moves a Point some amount of steps in a direction. This is the most used
way how to change a position.
-}
move : Int -> Direction -> Position -> Position
move n direction =
    let
        dir =
            direction |> fromDirection
    in
    add <| scaleBy n dir


{-| Apply Coordinates to a position to get the relative position.

```
move amount direction == add ( fromDirection direction |> scaleby amount)
```

-}
add : Coord -> Position -> Position
add v ( x, y ) =
    ( x + v.x
    , y + v.y
    )


{-| Scales Coordnates. Coordnates obtained by `fromDirection` have size 1.

```
fromDirection angle |> scaleBy l |> length == l
```

-}
scaleBy : Int -> Coord -> Coord
scaleBy n { x, y } =
    { x = x * n
    , y = y * n
    }


{-| Returns the difference between two positions.

```
difference p1 p2 == p1 <------- p2
```

-}
coordsTo : Position -> Position -> Coord
coordsTo ( x1, y1 ) ( x2, y2 ) =
    { x = x1 - x2
    , y = y1 - y2
    }


{-| Returns the length of the coordinate (distance to (0,0)
-}
length : Coord -> Float
length { x, y } =
    distance ( 0, 0 ) ( x, y )


{-| Gets the distance between to points.
(The length of the difference)

```
distance == vectorTo >> length
```

-}
distance : Position -> Position -> Float
distance p1 p2 =
    let
        { x, y } =
            coordsTo p1 p2
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
