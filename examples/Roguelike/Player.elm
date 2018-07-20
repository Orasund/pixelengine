module Roguelike.Player
    exposing
        ( Game
        , PlayerData
        , activate
        , attack
        , drop
        , face
        , getCell
        , init
        , move
        , rotateLeft
        , rotateRight
        )

import Dict
import Pair
import Roguelike.Cell as Cell
    exposing
        ( Cell(..)
        , ConsumableType(..)
        , EnemyType(..)
        , Item(..)
        , MaterialType(..)
        , SolidType(..)
        )
import Roguelike.Inventory as Inventory exposing (Inventory)
import Roguelike.Map as Map exposing (Direction(..), Map)
import Tuple.Extra as TupleExtra


type alias PlayerData =
    { inventory : Inventory Item
    , lifes : Int
    }


type alias Game =
    ( PlayerData, Map Cell )


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


move : Int -> Map.Location -> Direction -> Game -> Game
move worldSize location direction (( playerData, currentMap ) as game) =
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
        game
    else
        case currentMap |> Dict.get (Pair.map2 (+) location moveDir) of
            Just (Item item) ->
                let
                    ( maybeDroppedItem, inventory ) =
                        playerData.inventory |> Inventory.drop
                in
                ( { playerData
                    | inventory = inventory |> Inventory.add item
                  }
                , currentMap
                    |> Map.move location direction
                    |> (case maybeDroppedItem of
                            Just droppedItem ->
                                Map.place location (Item droppedItem)

                            _ ->
                                identity
                       )
                )

            Just (Enemy _ _) ->
                ( playerData
                , case
                    currentMap
                        |> Dict.get
                            ((moveDir
                                |> Pair.map ((*) 2)
                             )
                                |> Pair.map2 (+) location
                            )
                  of
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
                    ( Nothing, _ ) ->
                        ( playerData
                            |> addToInventory (Consumable (Material Dirt))
                        , currentMap
                            |> Dict.remove (Pair.map2 (+) location moveDir)
                        )

                    _ ->
                        game |> Tuple.mapSecond (face location direction)

            _ ->
                game |> Tuple.mapSecond (face location direction)


activate : Game -> Game
activate (( playerData, _ ) as game) =
    game
        |> (case playerData |> .inventory |> Inventory.selected of
                Just (Consumable consumable) ->
                    Tuple.mapFirst takeFromInventory
                        >> consumableAction consumable

                _ ->
                    identity
           )


consumableAction : ConsumableType -> Game -> Game
consumableAction consumable (( playerData, map ) as game) =
    let
        defaultCase : Game -> Game
        defaultCase =
            Tuple.mapFirst (addToInventory (Consumable consumable))
    in
    game
        |> ((case consumable of
                Bombe ->
                    bombeAction map

                Material material ->
                    materialAction map material

                HealthPotion ->
                    healthPotionAction playerData
            )
                |> Maybe.withDefault defaultCase
           )


healthPotionAction : PlayerData -> Maybe (Game -> Game)
healthPotionAction { lifes } =
    if lifes < 3 then
        Just
            (\game ->
                game
                    |> Tuple.mapFirst
                        (\playerData ->
                            { playerData | lifes = lifes + 1 }
                        )
            )
    else
        Nothing


bombeAction : Map Cell -> Maybe (Game -> Game)
bombeAction map =
    let
        specialCase : SolidType -> Maybe (Game -> Game)
        specialCase solidType =
            let
                ( maybeSolid, material ) =
                    Cell.decomposing solidType
            in
            maybeSolid
                |> Maybe.map
                    (\solid ->
                        \(( playerData, map ) as game) ->
                            ( playerData
                                |> addToInventory (Consumable (Material material))
                            , map
                                |> Map.place (posFront map) (Solid solid)
                            )
                    )

        --|> Maybe.withDefault game
    in
    placingItem map
        (Enemy PlacedBombe "")
        specialCase


materialAction : Map Cell -> MaterialType -> Maybe (Game -> Game)
materialAction map material =
    let
        specialCase : SolidType -> Maybe (Game -> Game)
        specialCase solidType =
            Just
                (case Cell.composing ( Just solidType, material ) of
                    Just newSolid ->
                        Tuple.mapSecond (Map.place (posFront map) (Solid newSolid))

                    Nothing ->
                        Tuple.mapFirst (addToInventory (Consumable (Material material)))
                )
    in
    case Cell.composing ( Nothing, material ) of
        Just solid ->
            placingItem
                map
                (Solid solid)
                specialCase

        Nothing ->
            Nothing


placingItem : Map Cell -> Cell -> (SolidType -> Maybe (Game -> Game)) -> Maybe (Game -> Game)
placingItem map cell specialCase =
    case map |> Dict.get (map |> posFront) of
        Nothing ->
            Just (Tuple.mapSecond (Map.place (posFront map) cell))

        Just (Effect _) ->
            Just (Tuple.mapSecond (Map.place (posFront map) cell))

        Just (Solid solidType) ->
            specialCase solidType

        _ ->
            Nothing


posFront : Map Cell -> ( Int, Int )
posFront map =
    getCell map
        |> Maybe.map
            (Tuple.mapSecond Map.dirCoordinates
                >> TupleExtra.apply (Pair.map2 (+))
            )
        |> Maybe.withDefault ( 0, 0 )


drop : Game -> Game
drop ( playerData, map ) =
    let
        ( item, inventory ) =
            playerData.inventory |> Inventory.take

        dir : Map.Location
        dir =
            getCell map
                |> Maybe.map
                    (\( location, direction ) ->
                        Pair.map2 (+) location (Map.dirCoordinates direction)
                    )
                |> Maybe.withDefault ( 0, 0 )
    in
    case map |> Dict.get dir of
        Nothing ->
            ( { playerData | inventory = inventory }
            , map
                |> (item
                        |> Maybe.map (\a -> Map.place dir (Item a))
                        |> Maybe.withDefault identity
                   )
            )

        _ ->
            ( playerData, map )


takeFromInventory : PlayerData -> PlayerData
takeFromInventory ({ inventory } as playerData) =
    { playerData
        | inventory = inventory |> Inventory.take |> Tuple.second
    }


addToInventory : Item -> PlayerData -> PlayerData
addToInventory item ({ inventory } as playerData) =
    { playerData
        | inventory =
            inventory
                |> Inventory.add item
    }


rotateLeft : PlayerData -> PlayerData
rotateLeft ({ inventory } as playerData) =
    { playerData
        | inventory = inventory |> Inventory.rotateLeft
    }


rotateRight : PlayerData -> PlayerData
rotateRight ({ inventory } as playerData) =
    { playerData
        | inventory = inventory |> Inventory.rotateRight
    }
