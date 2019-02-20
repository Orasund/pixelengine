module DigDigBoom.Game exposing (applyDirection)

import DigDigBoom.Cell as Cell
    exposing
        ( Cell(..)
        , EffectType(..)
        , EnemyType(..)
        , ItemType(..)
        , SolidType(..)
        )
import DigDigBoom.Component.Map as Map exposing (Actor)
import DigDigBoom.Player as Player exposing (Game)
import Grid as Grid
import Grid.Direction exposing (Direction(..))
import Grid.Position as Position exposing (Position)


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
        |> Grid.toList
        |> List.foldl
            (updateCell playerCell)
            game


updateCell : Actor -> ( Position, Cell ) -> Game -> Game
updateCell playerCell ( position, cell ) =
    case cell of
        Enemy enemy _ ->
            updateEnemy position enemy playerCell

        Effect _ ->
            Tuple.mapSecond (Grid.remove position)

        Stunned enemy id ->
            Tuple.mapSecond (Grid.update position (always <| Just <| Enemy enemy id))

        _ ->
            identity


updateEnemy : Position -> EnemyType -> Actor -> Game -> Game
updateEnemy position enemyType playerCell =
    attackPlayer position playerCell
        >> specialBehaviour position enemyType playerCell


attackPlayer : Position -> Actor -> Game -> Game
attackPlayer location (( playerLocation, _ ) as playerCell) ( playerData, map ) =
    [ Up, Down, Left, Right ]
        |> List.filter
            (Position.fromDirection >> (==) (playerLocation |> Position.coordsTo location))
        |> List.head
        |> Maybe.map (always (( playerData, map ) |> Player.attack playerCell))
        |> Maybe.withDefault ( playerData, map )


specialBehaviour : Position -> EnemyType -> Actor -> Game -> Game
specialBehaviour currentLocation enemyType ( playerLocation, _ ) (( _, map ) as game) =
    case enemyType of
        PlacedBombe ->
            [ Up, Down, Left, Right ]
                |> List.foldl
                    (placedBombeBehavoiur currentLocation)
                    game
                |> Tuple.mapSecond
                    (Grid.update currentLocation (always (Just (Effect Smoke))))

        monster ->
            let
                moveDirection : Direction
                moveDirection =
                    currentLocation
                        |> Position.coordsTo playerLocation
                        |> Position.toDirection

                actor : Actor
                actor =
                    ( currentLocation, moveDirection )

                newLocation : Position
                newLocation =
                    actor |> Map.posFront 1
            in
            game
                |> (case map |> Grid.get newLocation of
                        Nothing ->
                            Tuple.mapSecond
                                (Map.move actor)

                        Just (Item _) ->
                            Tuple.mapSecond
                                (Map.move actor)

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
                                    Grid.update newLocation <|
                                        always (Cell.decomposing solid |> Tuple.first |> Maybe.map Solid)

                            else
                                identity

                        _ ->
                            identity
                   )


placedBombeBehavoiur : Position -> Direction -> Game -> Game
placedBombeBehavoiur location direction game =
    let
        newLocation =
            ( location, direction ) |> Map.posFront 1
    in
    game
        |> Tuple.mapSecond
            (Grid.update
                newLocation
                (\elem ->
                    case elem of
                        Just (Enemy _ _) ->
                            Just <| Effect Bone

                        Nothing ->
                            Just <| Effect Smoke

                        _ ->
                            elem
                )
            )
