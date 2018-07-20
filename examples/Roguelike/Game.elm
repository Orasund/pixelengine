module Roguelike.Game exposing (applyDirection)

import Dict
import Pair
import Roguelike.Cell as Cell
    exposing
        ( Cell(..)
        , ConsumableType(..)
        , EffectType(..)
        , EnemyType(..)
        , Item(..)
        , MiscellaneousType(..)
        , SolidType(..)
        )
import Roguelike.Map as Map exposing (Direction(..), Location, Map)
import Roguelike.Player as Player exposing (Game, PlayerData)


applyDirection : Int -> Direction -> Game -> Game
applyDirection size dir ( playerData, map ) =
    case player map of
        Just ( location, d ) ->
            if d == dir then
                ( playerData, map )
                    |> Player.move size location d
                    |> (\tuple ->
                            tuple
                                |> Tuple.second
                                |> Dict.foldl
                                    updateCell
                                    tuple
                       )
            else
                ( playerData
                , map |> Player.face location dir
                )

        Nothing ->
            ( playerData, map )


updateCell : Location -> Cell -> Game -> Game
updateCell location cell game =
    case cell of
        Enemy enemy id ->
            game |> updateEnemy location enemy id

        Effect _ ->
            game
                |> Tuple.mapSecond (Dict.remove location)

        _ ->
            game


updateEnemy : Location -> EnemyType -> String -> Game -> Game
updateEnemy location enemyType id game =
    game
        |> attackPlayer location
        |> specialBehaviour location enemyType


player : Map Cell -> Maybe ( Location, Direction )
player map =
    Player.getCell map


attackPlayer : Location -> Game -> Game
attackPlayer location ( playerData, map ) =
    case player map of
        Just ( playerLocation, _ ) ->
            [ Up, Down, Left, Right ]
                |> List.filter
                    ((==) (Pair.map2 (-) location playerLocation) << Map.dirCoordinates)
                |> List.head
                |> Maybe.map (always (( playerData, map ) |> Tuple.mapFirst Player.attack))
                |> Maybe.withDefault ( playerData, map )

        Nothing ->
            ( playerData, map )


specialBehaviour : Location -> EnemyType -> Game -> Game
specialBehaviour currentLocation enemyType game =
    let
        map : Map Cell
        map =
            game |> Tuple.second
    in
    case enemyType of
        PlacedBombe ->
            [ Up, Down, Left, Right ]
                |> List.foldl
                    (placedBombeBehavoiur currentLocation)
                    game
                |> Tuple.mapSecond
                    (Dict.update currentLocation (always (Just (Effect Smoke))))

        monster ->
            case player map of
                Just ( playerLocation, _ ) ->
                    let
                        moveDirection : Direction
                        moveDirection =
                            Pair.map2 (-) playerLocation currentLocation
                                --|> (\( x, y ) -> ( y, x ))
                                |> Map.approximateDirection

                        newLocation : Location
                        newLocation =
                            Pair.map2 (+) currentLocation (Map.dirCoordinates moveDirection)
                    in
                    case map |> Dict.get newLocation of
                        Nothing ->
                            game |> Tuple.mapSecond (Map.move currentLocation moveDirection)

                        Just (Item _) ->
                            game |> Tuple.mapSecond (Map.move currentLocation moveDirection)

                        Just (Solid solid) ->
                            if
                                Cell.resistancy solid
                                    <= (case monster of
                                            PlacedBombe ->
                                                0

                                            Oger ->
                                                3

                                            Goblin ->
                                                2

                                            Rat ->
                                                1
                                       )
                            then
                                game
                                    |> Tuple.mapSecond
                                        (Dict.update newLocation (always (Cell.decomposing solid |> Tuple.first |> Maybe.map Solid)))
                            else
                                game

                        _ ->
                            game

                Nothing ->
                    game


placedBombeBehavoiur : Location -> Direction -> Game -> Game
placedBombeBehavoiur currentLocation dir game =
    let
        newLocation =
            Map.dirCoordinates dir |> Pair.map2 (+) currentLocation
    in
    case game |> Tuple.second |> Dict.get newLocation of
        Just (Enemy _ _) ->
            game
                |> Tuple.mapSecond
                    (Dict.update newLocation (always (Just (Item (Miscellaneous Bone)))))

        Nothing ->
            game
                |> Tuple.mapSecond
                    (Dict.update newLocation (always (Just (Effect Smoke))))

        _ ->
            game
