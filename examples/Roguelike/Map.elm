module Roguelike.Map exposing (Direction(..), Location, Map, approximateDirection, dirCoordinates, generate, getUnique, move, place, remove)

import Dict exposing (Dict)
import Pair
import Random


type alias Location =
    ( Int, Int )


type Direction
    = Up
    | Down
    | Left
    | Right


type alias Map a =
    Dict Location a


generate : Int -> (Location -> ( Map a, Random.Seed ) -> ( Map a, Random.Seed )) -> Random.Seed -> ( Map a, Random.Seed )
generate size fun seed =
    List.range 0 size
        |> List.foldl
            (\x out ->
                List.range 0 size
                    |> List.foldl
                        (\y tuple -> fun ( x, y ) tuple)
                        out
            )
            ( Dict.empty, seed )


move : Location -> Direction -> Map a -> Map a
move pos dir map =
    case map |> Dict.get pos of
        Just a ->
            map
                |> Dict.update (Pair.map2 (+) pos (dirCoordinates dir)) (always (Just a))
                |> Dict.remove pos

        Nothing ->
            map


approximateDirection : Location -> Direction
approximateDirection ( a, b ) =
    if abs a > abs b then
        if a > 0 then
            Right
        else
            Left
    else if b > 0 then
        Down
    else
        Up


dirCoordinates : Direction -> ( Int, Int )
dirCoordinates direction =
    case direction of
        Up ->
            ( 0, -1 )

        Down ->
            ( 0, 1 )

        Left ->
            ( -1, 0 )

        Right ->
            ( 1, 0 )


place : Location -> a -> Map a -> Map a
place location cell map =
    map
        |> Dict.update location (always (Just cell))


remove : Location -> Map a -> Map a
remove location map =
    map |> Dict.remove location


getUnique : (Location -> a -> Bool) -> Map a -> Maybe ( Location, a )
getUnique fun map =
    map
        |> Dict.filter fun
        |> Dict.toList
        |> List.head
