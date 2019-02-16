module DigDigBoom.Component.Map exposing
    ( Actor
    , generator
    , move
    , posFront
    )

import Dict exposing (Dict)
import PixelEngine.Grid as Grid exposing (Grid)
import PixelEngine.Grid.Direction as Direction exposing (Direction(..))
import PixelEngine.Grid.Position as Position exposing (Position, Vector)
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
                    ( (case maybeA pos of
                                Just a ->
                                    case grid |> Grid.insert pos a of
                                        Ok value ->
                                            value
                                        Err _ ->
                                            grid

                                Nothing ->
                                    grid
                           )
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


move : Actor -> Grid a -> Result () (Grid a)
move ( pos, dir ) grid =
    case grid |> Grid.get pos |> Result.withDefault Nothing of
        Just a ->
            grid
                |> Grid.insert
                    (pos |> Position.move 1 dir)
                    a
                |> Result.andThen (Grid.remove pos)
                |> Result.mapError (always ())
        Nothing ->
            Ok grid