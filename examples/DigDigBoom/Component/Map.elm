module DigDigBoom.Component.Map exposing
    ( Actor
    , generator
    , move
    , posFront
    )

import Grid as Grid exposing (Grid)
import Grid.Direction exposing (Direction(..))
import Grid.Position as Position exposing (Position)
import Random exposing (Generator)
import Result exposing (Result(..))


type alias Actor =
    ( Position, Direction )


posFront : Int -> Actor -> Position
posFront n ( position, direction ) =
    position |> Position.move n direction


generator : Int -> Generator (Position -> Maybe a) -> Generator (Grid a)
generator size fun =
    Random.list (size * size) fun
        |> Random.map
            (List.foldl
                (\maybeA ( grid, xy ) ->
                    let
                        pos : Position
                        pos =
                            ( modBy size xy
                            , xy // size
                            )
                    in
                    ( case maybeA pos of
                        Just a ->
                            grid |> Grid.insert pos a

                        Nothing ->
                            grid
                    , xy + 1
                    )
                )
                ( Grid.empty
                    { rows = size
                    , columns = size
                    }
                , 0
                )
                >> Tuple.first
            )


move : Actor -> Grid a -> Grid a
move ( pos, dir ) grid =
    case grid |> Grid.get pos of
        Just a ->
            grid
                |> Grid.insert
                    (pos |> Position.move 1 dir)
                    a
                |> Grid.remove pos

        Nothing ->
            grid
