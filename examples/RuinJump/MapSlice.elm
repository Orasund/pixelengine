module RuinJump.MapSlice exposing (generator)

import Dict exposing (Dict)
import List.Zipper as Zipper exposing (Zipper)
import Random exposing (Generator)


generateWeights : List a -> Generator (List Int)
generateWeights list =
    Random.list
        (list |> List.length)
        (Random.int Random.minInt Random.maxInt)


generator : Int -> Dict ( Int, Int ) a -> Generator (Zipper Int)
generator lowestY map =
    let
        orderedSlice : List ( ( Int, Int ), a )
        orderedSlice =
            map
                |> Dict.filter (\( _, y ) _ -> y == lowestY)
                |> Dict.toList

        shuffle : List Int -> List ( ( Int, Int ), a ) -> Zipper Int
        shuffle weights =
            List.map2
                (\s ( ( x, _ ), _ ) -> ( x, s ))
                weights
                >> List.sortBy Tuple.second
                >> List.map Tuple.first
                >> Zipper.fromList
                >> Zipper.withDefault 0
    in
    orderedSlice
        |> generateWeights
        |> Random.map
            (\weights ->
                orderedSlice
                    |> shuffle weights
            )
