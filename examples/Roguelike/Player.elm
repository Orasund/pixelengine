module Roguelike.Player exposing (PlayerData, activate, attack, drop, face, getCell, init, move, rotateLeft, rotateRight)

import Dict
import Pair
import Roguelike.Cell as Cell exposing (Cell(..), ConsumableType(..), EnemyType(..), Item(..), MaterialType(..), SolidType(..))
import Roguelike.Inventory as Inventory exposing (Inventory)
import Roguelike.Map as Map exposing (Direction(..), Map)


type alias PlayerData =
    { inventory : Inventory Item
    , lifes : Int
    }


init : Int -> PlayerData
init backpackSize =
    { inventory = Inventory.init backpackSize
    , lifes = 3
    }


getCell : Map Cell -> Maybe ( Map.Location, Direction )
getCell map =
    map
        |> Map.getUnique
            (\_ cell ->
                case cell of
                    Player _ ->
                        True

                    _ ->
                        False
            )
        |> Maybe.andThen
            (\( key, cell ) ->
                case cell of
                    Player dir ->
                        Just ( key, dir )

                    _ ->
                        Nothing
            )


face : Map.Location -> Direction -> Map Cell -> Map Cell
face location direction map =
    map |> Map.place location (Player direction)


attack : PlayerData -> PlayerData
attack player =
    { player | lifes = player.lifes - 1 }


move : Int -> Map.Location -> Direction -> ( PlayerData, Map Cell ) -> ( PlayerData, Map Cell )
move worldSize location direction ( playerData, currentMap ) =
    let
        moveDir =
            Map.dirCoordinates direction

        outOfBound : Bool
        outOfBound =
            location
                |> (\( x, y ) ->
                        case direction of
                            Up ->
                                y == 0

                            Down ->
                                y == worldSize

                            Left ->
                                x == 0

                            Right ->
                                x == worldSize
                   )
    in
    if outOfBound then
        ( playerData, currentMap )
    else
        case currentMap |> Dict.get (Pair.map2 (+) location moveDir) of
            Just (Item a) ->
                let
                    ( item, inventory ) =
                        playerData.inventory |> Inventory.drop
                in
                ( playerData
                    |> (\b ->
                            { b
                                | inventory = inventory |> Inventory.add a
                            }
                       )
                , currentMap
                    |> Map.move location direction
                    |> (\m ->
                            case item of
                                Just c ->
                                    m |> Map.place location (Item c)

                                _ ->
                                    m
                       )
                )

            Just (Enemy _ id) ->
                ( playerData
                , case currentMap |> Dict.get ((moveDir |> Pair.map ((*) 2)) |> Pair.map2 (+) location) of
                    Just (Solid _) ->
                        currentMap

                    Just (Enemy _ _) ->
                        currentMap

                    _ ->
                        currentMap
                            |> Map.move (Pair.map2 (+) location moveDir) direction
                            |> (case currentMap |> Dict.get ((moveDir |> Pair.map ((*) 3)) |> Pair.map2 (+) location) of
                                    Just (Solid _) ->
                                        identity

                                    Just (Enemy _ _) ->
                                        identity

                                    _ ->
                                        Map.move (Pair.map2 (+) location (Pair.map ((*) 2) moveDir)) direction
                               )
                )

            Nothing ->
                let
                    ( item, inventory ) =
                        playerData.inventory |> Inventory.drop
                in
                ( playerData |> (\a -> { a | inventory = inventory })
                , currentMap
                    |> Map.move location direction
                    |> (\m ->
                            case item of
                                Just a ->
                                    m |> Map.place location (Item a)

                                Nothing ->
                                    m
                       )
                )

            Just (Effect _) ->
                let
                    ( item, inventory ) =
                        playerData.inventory |> Inventory.drop
                in
                ( playerData |> (\a -> { a | inventory = inventory })
                , currentMap
                    |> Map.move location direction
                    |> (\m ->
                            case item of
                                Just a ->
                                    m |> Map.place location (Item a)

                                Nothing ->
                                    m
                       )
                )

            Just (Solid solid) ->
                case Cell.decomposing solid of
                    ( Nothing, material ) ->
                        ( playerData
                            |> (\pd ->
                                    { pd
                                        | inventory =
                                            pd.inventory
                                                |> Inventory.add (Consumable (Material Dirt))
                                    }
                               )
                        , currentMap
                            |> Dict.remove (Pair.map2 (+) location moveDir)
                        )

                    _ ->
                        ( playerData
                        , currentMap
                            |> face location direction
                        )

            _ ->
                ( playerData
                , currentMap
                    |> face location direction
                )


activate : ( PlayerData, Map Cell ) -> ( PlayerData, Map Cell )
activate ( playerData, map ) =
    case playerData.inventory |> Inventory.selected of
        Just (Consumable a) ->
            ( playerData
                |> (\b ->
                        { b
                            | inventory =
                                playerData.inventory
                                    |> Inventory.take
                                    |> Tuple.second
                        }
                   )
            , map
            )
                |> (\tuple ->
                        case a of
                            Bombe ->
                                let
                                    pos : ( Int, Int )
                                    pos =
                                        case getCell (tuple |> Tuple.second) of
                                            Just ( ( x, y ), dir ) ->
                                                let
                                                    ( i, j ) =
                                                        Map.dirCoordinates dir
                                                in
                                                ( x + i, y + j )

                                            Nothing ->
                                                ( 0, 0 )
                                in
                                case tuple |> Tuple.second |> Dict.get pos of
                                    Nothing ->
                                        tuple
                                            |> Tuple.mapSecond (Map.place pos (Enemy PlacedBombe ""))

                                    Just (Effect _) ->
                                        tuple
                                            |> Tuple.mapSecond (Map.place pos (Enemy PlacedBombe ""))

                                    Just (Solid solidType) ->
                                        let
                                            ( maybeSolid, material ) =
                                                Cell.decomposing solidType
                                        in
                                        case maybeSolid of
                                            Just solid ->
                                                ( tuple
                                                    |> Tuple.first
                                                    |> (\pd ->
                                                            { pd
                                                                | inventory =
                                                                    pd.inventory
                                                                        |> Inventory.add (Consumable (Material material))
                                                            }
                                                       )
                                                , tuple
                                                    |> Tuple.second
                                                    |> Map.place pos (Solid solid)
                                                )

                                            Nothing ->
                                                ( tuple
                                                    |> Tuple.first
                                                    |> (\pd ->
                                                            { pd
                                                                | inventory =
                                                                    pd.inventory
                                                                        |> Inventory.add (Consumable Bombe)
                                                            }
                                                       )
                                                , tuple
                                                    |> Tuple.second
                                                )

                                    _ ->
                                        ( tuple
                                            |> Tuple.first
                                            |> (\pd ->
                                                    { pd
                                                        | inventory =
                                                            pd.inventory
                                                                |> Inventory.add (Consumable Bombe)
                                                    }
                                               )
                                        , tuple
                                            |> Tuple.second
                                        )

                            Material material ->
                                let
                                    pos : ( Int, Int )
                                    pos =
                                        case getCell (tuple |> Tuple.second) of
                                            Just ( ( x, y ), dir ) ->
                                                let
                                                    ( i, j ) =
                                                        Map.dirCoordinates dir
                                                in
                                                ( x + i, y + j )

                                            Nothing ->
                                                ( 0, 0 )
                                in
                                case Cell.composing ( Nothing, material ) of
                                    Just solid ->
                                        case tuple |> Tuple.second |> Dict.get pos of
                                            Nothing ->
                                                tuple
                                                    |> Tuple.mapSecond (Map.place pos (Solid solid))

                                            Just (Effect _) ->
                                                tuple
                                                    |> Tuple.mapSecond (Map.place pos (Solid solid))

                                            Just (Solid placedSolid) ->
                                                case Cell.composing ( Just placedSolid, material ) of
                                                    Just newSolid ->
                                                        tuple
                                                            |> Tuple.mapSecond (Map.place pos (Solid newSolid))

                                                    Nothing ->
                                                        ( tuple
                                                            |> Tuple.first
                                                            |> (\pd ->
                                                                    { pd
                                                                        | inventory =
                                                                            pd.inventory
                                                                                |> Inventory.add (Consumable (Material material))
                                                                    }
                                                               )
                                                        , tuple
                                                            |> Tuple.second
                                                        )

                                            _ ->
                                                ( tuple
                                                    |> Tuple.first
                                                    |> (\pd ->
                                                            { pd
                                                                | inventory =
                                                                    pd.inventory
                                                                        |> Inventory.add (Consumable (Material material))
                                                            }
                                                       )
                                                , tuple
                                                    |> Tuple.second
                                                )

                                    Nothing ->
                                        ( tuple
                                            |> Tuple.first
                                            |> (\pd ->
                                                    { pd
                                                        | inventory =
                                                            pd.inventory
                                                                |> Inventory.add (Consumable (Material material))
                                                    }
                                               )
                                        , tuple
                                            |> Tuple.second
                                        )

                            _ ->
                                tuple
                   )

        _ ->
            ( playerData, map )


drop : ( PlayerData, Map Cell ) -> ( PlayerData, Map Cell )
drop ( playerData, map ) =
    let
        ( item, inventory ) =
            playerData.inventory |> Inventory.take

        dir : Map.Location
        dir =
            case getCell map of
                Just ( ( x, y ), direction ) ->
                    let
                        ( i, j ) =
                            Map.dirCoordinates direction
                    in
                    ( x + i, y + j )

                Nothing ->
                    ( 0, 0 )
    in
    case map |> Dict.get dir of
        Nothing ->
            ( playerData |> (\a -> { a | inventory = inventory })
            , case item of
                Just a ->
                    map |> Map.place dir (Item a)

                _ ->
                    map
            )

        _ ->
            ( playerData, map )


rotateLeft : ( PlayerData, Map Cell ) -> ( PlayerData, Map Cell )
rotateLeft ( playerData, map ) =
    ( { playerData
        | inventory =
            playerData.inventory
                |> Inventory.rotateLeft
      }
    , map
    )


rotateRight : ( PlayerData, Map Cell ) -> ( PlayerData, Map Cell )
rotateRight ( playerData, map ) =
    ( { playerData
        | inventory =
            playerData.inventory
                |> Inventory.rotateRight
      }
    , map
    )
