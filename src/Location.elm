module Location exposing
    ( Angle(..)
    , Location
    , Vector
    , add
    , difference
    , distance
    , fromAngle
    , length
    , move
    , rotate
    , scaleBy
    , toAngle
    )


type Angle
    = Angle Float


type alias Location =
    ( Float, Float )


type alias Vector =
    { x : Float
    , y : Float
    }


add : Vector -> Location -> Location
add v ( x, y ) =
    ( x + v.x
    , y + v.y
    )


scaleBy : Float -> Vector -> Vector
scaleBy n { x, y } =
    { x = x * n
    , y = y * n
    }


difference : Location -> Location -> Vector
difference ( x1, y1 ) ( x2, y2 ) =
    { x = x1 - x2
    , y = y1 - y2
    }


{-| returns the length of the coordinate (distance to (0,0)
-}
length : Vector -> Float
length { x, y } =
    distance ( 0, 0 ) ( x, y )


distance : Location -> Location -> Float
distance p1 p2 =
    let
        { x, y } =
            difference p1 p2
    in
    sqrt <| (x * x + y * y)


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


fromAngle : Angle -> Vector
fromAngle angle =
    { x = 1
    , y = 0
    }
        |> rotate angle


toAngle : Vector -> Angle
toAngle { x, y } =
    let
        ( _, r ) =
            toPolar ( x, y )
    in
    Angle r


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
