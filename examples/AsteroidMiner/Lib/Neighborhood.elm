module AsteroidMiner.Lib.Neighborhood exposing (Neighborhood, filter, fromPosition, map, toList)

import Grid.Bordered as Grid exposing (Error, Grid)
import Grid.Direction exposing (Direction(..))
import Grid.Position as Position exposing (Position)


type alias Neighborhood a =
    { up : a
    , left : a
    , right : a
    , down : a
    }


toList : Neighborhood a -> List ( Direction, a )
toList { up, left, right, down } =
    [ ( Up, up ), ( Left, left ), ( Right, right ), ( Down, down ) ]


filter : (a -> Bool) -> Neighborhood (Maybe a) -> List ( Direction, Maybe a )
filter fun =
    toList
        >> List.filter
            (Tuple.second
                >> Maybe.map fun
                >> Maybe.withDefault False
            )


fromPosition : Position -> Grid a -> Result Error ( Maybe a, Neighborhood (Maybe a) )
fromPosition pos grid =
    let
        get : Direction -> Maybe a
        get direction =
            grid
                |> Grid.get (pos |> Position.move 1 direction)
                |> Result.withDefault Nothing
    in
    grid
        |> Grid.get pos
        |> Result.andThen
            (\a ->
                Ok
                    ( a
                    , { up = get Up
                      , left = get Left
                      , right = get Right
                      , down = get Down
                      }
                    )
            )


map : (a -> b) -> Neighborhood a -> Neighborhood b
map fun { up, left, right, down } =
    { up = fun up
    , left = fun left
    , right = fun right
    , down = fun down
    }
