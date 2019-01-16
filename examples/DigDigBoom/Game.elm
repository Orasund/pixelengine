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
import DigDigBoom.Component.Map as Map exposing (Actor, Direction(..), Location)
import DigDigBoom.Player as Player exposing (Game)


applyDirection : Int -> Direction -> ( Actor, Game ) -> ( Actor, Game )
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


updateGame : Actor -> Game -> Game
updateGame playerCell (( _, map ) as game) =
    map
        |> Dict.foldl
            (updateCell playerCell)
            game


updateCell : Actor -> Location -> Cell -> Game -> Game
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


updateEnemy : Location -> EnemyType -> Actor -> Game -> Game
updateEnemy location enemyType playerCell =
    attackPlayer location playerCell
        >> specialBehaviour location enemyType playerCell


attackPlayer : Location -> Actor -> Game -> Game
attackPlayer location (( playerLocation, _ ) as playerCell) ( playerData, map ) =
    [ Up, Down, Left, Right ]
        |> List.filter
            ((==) ((\( x1, y1 ) ( x2, y2 ) -> ( x1 - x2, y1 - y2 )) location playerLocation) << Map.dirCoordinates)
        |> List.head
        |> Maybe.map (always (( playerData, map ) |> Player.attack playerCell))
        |> Maybe.withDefault ( playerData, map )


specialBehaviour : Location -> EnemyType -> Actor -> Game -> Game
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
                    (\( x1, y1 ) ( x2, y2 ) -> ( x1 - x2, y1 - y2 )) playerLocation currentLocation
                        --|> (\( x, y ) -> ( y, x ))
                        |> Map.approximateDirection

                actor : Actor
                actor =
                    ( currentLocation, moveDirection )

                newLocation : Location
                newLocation =
                    actor |> Map.posFront 1
            in
            game
                |> (case map |> Dict.get newLocation of
                        Nothing ->
                            Tuple.mapSecond (Map.move actor)

                        Just (Item _) ->
                            Tuple.mapSecond (Map.move actor)

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
placedBombeBehavoiur location direction game =
    let
        newLocation =
            ( location, direction ) |> Map.posFront 1
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
