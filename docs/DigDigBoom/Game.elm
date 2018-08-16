module DigDigBoom.Game exposing (applyDirection)

import Dict
import DigDigBoom.Cell as Cell
    exposing
        ( Cell(..)
        , EffectType(..)
        , EnemyType(..)
        , ItemType(..)
        , SolidType(..)
        )
import DigDigBoom.Component.Map as Map exposing (Direction(..), Location)
import DigDigBoom.Player as Player exposing (Game, PlayerCell)
import Pair


applyDirection : Int -> Direction -> ( PlayerCell, Game ) -> ( PlayerCell, Game )
applyDirection size dir (( ( location, direction ), _ ) as playerCellAndGame) =
    if direction == dir then
        playerCellAndGame
            |> Player.move size
            |> (\( newPlayerCell, game ) ->
                    ( newPlayerCell
                    , updateGame newPlayerCell game
                    )
               )
    else
        playerCellAndGame
            |> Tuple.mapSecond (Tuple.mapSecond (Player.face location dir))


updateGame : PlayerCell -> Game -> Game
updateGame playerCell (( _, map ) as game) =
    map
        |> Dict.foldl
            (updateCell playerCell)
            game


updateCell : PlayerCell -> Location -> Cell -> Game -> Game
updateCell playerCell location cell =
    case cell of
        Enemy enemy _ ->
            updateEnemy location enemy playerCell

        Effect _ ->
            Tuple.mapSecond (Dict.remove location)

        Stunned enemy id ->
            Tuple.mapSecond (Dict.update location (always <| Just <| Enemy enemy id))

        _ ->
            identity


updateEnemy : Location -> EnemyType -> PlayerCell -> Game -> Game
updateEnemy location enemyType playerCell =
    attackPlayer location playerCell
        >> specialBehaviour location enemyType playerCell


attackPlayer : Location -> PlayerCell -> Game -> Game
attackPlayer location (( playerLocation, _ ) as playerCell) ( playerData, map ) =
    [ Up, Down, Left, Right ]
        |> List.filter
            ((==) (Pair.map2 (-) location playerLocation) << Map.dirCoordinates)
        |> List.head
        |> Maybe.map (always (( playerData, map ) |> Player.attack playerCell))
        |> Maybe.withDefault ( playerData, map )


specialBehaviour : Location -> EnemyType -> PlayerCell -> Game -> Game
specialBehaviour currentLocation enemyType ( playerLocation, _ ) (( _, map ) as game) =
    case enemyType of
        PlacedBombe ->
            [ Up, Down, Left, Right ]
                |> List.foldl
                    (placedBombeBehavoiur currentLocation)
                    game
                |> Tuple.mapSecond
                    (Dict.update currentLocation (always (Just (Effect Smoke))))

        monster ->
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
            game
                |> (case map |> Dict.get newLocation of
                        Nothing ->
                            Tuple.mapSecond (Map.move currentLocation moveDirection)

                        Just (Item _) ->
                            Tuple.mapSecond (Map.move currentLocation moveDirection)

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
                                Tuple.mapSecond <|
                                    Dict.update newLocation <|
                                        always (Cell.decomposing solid |> Tuple.first |> Maybe.map Solid)
                            else
                                identity

                        _ ->
                            identity
                   )


placedBombeBehavoiur : Location -> Direction -> Game -> Game
placedBombeBehavoiur currentLocation dir game =
    let
        newLocation =
            Map.dirCoordinates dir |> Pair.map2 (+) currentLocation
    in
    game
        |> (case game |> Tuple.second |> Dict.get newLocation of
                Just (Enemy _ _) ->
                    Tuple.mapSecond
                        (Dict.update newLocation <| always <| Just <| Effect Bone)

                Nothing ->
                    Tuple.mapSecond
                        (Dict.update newLocation <| always <| Just <| Effect Smoke)

                _ ->
                    identity
           )
