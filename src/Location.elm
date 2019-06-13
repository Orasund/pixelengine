module Location exposing
    ( Angle(..), toAngle
    , Location, move, add, vectorTo
    , Vector, fromAngle, distance, rotate, scaleBy, length
    )

{-| DEPRECATED!
Got moved to its [separate package](https://package.elm-lang.org/packages/Orasund/elm-game-essentials/latest/).

This module contains two different type for repesenting a 2D Point:
`Location` and `Vector`. The idea is to use `Locations` to store a Point and `Vector`
to do caluclations. This way the compiler knows exactly what you are doing and can
give you appropriate error messages.


# Angle

@docs Angle, toAngle


# Location

@docs Location, move, add, vectorTo


# Vector

@docs Vector, fromAngle, distance, rotate, scaleBy, length

-}


{-| A Angle should store Elm angles (radians).

Use the functions `degrees`,`radias`,`turns` or `pi` to obtain a Angle.

    Angle <| degree 90

-}
type Angle
    = Angle Float


{-| An Angle can be constructed from a Vector.

    { x = 0, y = 0 } |> Location.toAngle == 0

-}
toAngle : Vector -> Angle
toAngle { x, y } =
    let
        ( _, r ) =
            toPolar ( x, y )
    in
    Angle r


{-| A `Location` a point, possibly stored in a dictionary. Thats why its a tuple.
But doing calulations with tuples can get very chaotic, thats why the module provides
`Vectors`.
-}
type alias Location =
    ( Float, Float )


{-| given a `Location`, a distance in some direction. This is the most used
way how to change a location.
-}
move : Float -> Angle -> Location -> Location
move l angle =
    let
        dir =
            angle |> fromAngle
    in
    add
        { x = dir.x * l
        , y = dir.y * l
        }


{-| simple adds a vector to a location, from geometry we know that a vector is
actually just an angle and a length.

    move length angle == add (fromAngle angle |> scaleby length)

-}
add : Vector -> Location -> Location
add v ( x, y ) =
    ( x + v.x
    , y + v.y
    )


{-| The difference between to locations is a vector.

    loc1 <----- loc2

the resulting vector points from the second value to the first.

-}
vectorTo : Location -> Location -> Vector
vectorTo ( x1, y1 ) ( x2, y2 ) =
    { x = x1 - x2
    , y = y1 - y2
    }


{-| A Vector is used for calculations.
-}
type alias Vector =
    { x : Float
    , y : Float
    }


{-| construct a unit vector from an angle

    fromAngle >> length == 1

-}
fromAngle : Angle -> Vector
fromAngle angle =
    { x = 1
    , y = 0
    }
        |> rotate angle


{-| Scales a Vector. Vectors obtained by `fromAngle` have size 1.

    fromAngle angle |> scaleBy l |> length == l

-}
scaleBy : Float -> Vector -> Vector
scaleBy n { x, y } =
    { x = x * n
    , y = y * n
    }


{-| rotate a Vector
-}
rotate : Angle -> Vector -> Vector
rotate (Angle angle) { x, y } =
    let
        ( l, r ) =
            ( x, y ) |> toPolar

        ( newX, newY ) =
            fromPolar ( l, r + angle )
    in
    { x = newX
    , y = newY
    }


{-| Returns the length of the coordinate (distance to (0,0)
-}
length : Vector -> Float
length { x, y } =
    distance ( 0, 0 ) ( x, y )


{-| The `Distance` between two locations is the length of the resulting vector

    distance == vectorTo >> length

-}
distance : Location -> Location -> Float
distance p1 p2 =
    let
        { x, y } =
            vectorTo p1 p2
    in
    sqrt <| (x * x + y * y)
