module DigDigBoom.Player exposing
    ( Game
    , PlayerData
    , activate
    , attack
    , drop
    , face
    , init
    , move
    , rotateLeft
    , rotateRight
    )

import Dict
import DigDigBoom.Cell as Cell
    exposing
        ( Cell(..)
        , EffectType(..)
        , EnemyType(..)
        , ItemType(..)
        , MaterialType
        , SolidType(..)
        )
import DigDigBoom.Component.Inventory as Inventory exposing (Inventory)
import DigDigBoom.Component.Map as Map exposing (Actor, Direction(..), Location, Map)


type alias PlayerData =
    { inventory : Inventory ItemType
    , lifes : Int
    }


type alias Game =
    ( PlayerData, Map Cell )


init : Int -> PlayerData
init backpackSize =
    { inventory = Inventory.init backpackSize
    , lifes = 3
    }


face : Location -> Direction -> Map Cell -> Map Cell
face location direction map =
    map |> Map.place location (Player direction)


attack : Actor -> Game -> Game
attack ( location, _ ) ( playerData, currentMap ) =
    let
        lifes : Int
        lifes =
            playerData.lifes - 1
    in
    ( { playerData | lifes = lifes }
    , currentMap
        |> (if lifes > 0 then
                identity

            else
                Dict.update location <| always <| Just <| Effect Bone
           )
    )


move : Int -> ( Actor, Game ) -> ( Actor, Game )
move worldSize ( ( location, direction ) as playerCell, ( playerData, currentMap ) as game ) =
    let
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

        newLocation : Location
        newLocation =
            playerCell |> Map.posFront 1

        newPlayerCell : Actor
        newPlayerCell =
            playerCell |> Tuple.mapFirst (always newLocation)
    in
    if outOfBound then
        ( playerCell, game )

    else
        case currentMap |> Dict.get newLocation of
            Just (Item item) ->
                let
                    ( maybeDroppedItem, inventory ) =
                        playerData.inventory |> Inventory.drop
                in
                ( newPlayerCell
                , ( { playerData
                        | inventory = inventory |> Inventory.add item
                    }
                  , currentMap
                        |> Map.move playerCell
                        |> (case maybeDroppedItem of
                                Just droppedItem ->
                                    Map.place location (Item droppedItem)

                                _ ->
                                    identity
                           )
                  )
                )

            Just (Enemy enemy id) ->
                ( playerCell, game )
                    |> Tuple.mapSecond (Tuple.mapSecond (throwEnemy playerCell enemy id))

            Nothing ->
                let
                    ( item, inventory ) =
                        playerData.inventory |> Inventory.drop
                in
                ( newPlayerCell
                , ( playerData |> (\a -> { a | inventory = inventory })
                  , currentMap
                        |> Map.move playerCell
                        |> (\m ->
                                case item of
                                    Just a ->
                                        m |> Map.place location (Item a)

                                    Nothing ->
                                        m
                           )
                  )
                )

            Just (Effect _) ->
                let
                    ( item, inventory ) =
                        playerData.inventory |> Inventory.drop
                in
                ( newPlayerCell
                , ( playerData |> (\a -> { a | inventory = inventory })
                  , currentMap
                        |> Map.move playerCell
                        |> (case item of
                                Just a ->
                                    Map.place location (Item a)

                                Nothing ->
                                    identity
                           )
                  )
                )

            Just (Solid solid) ->
                ( playerCell
                , case Cell.decomposing solid of
                    ( Nothing, material ) ->
                        ( playerData
                            |> (addToInventory <| Material material)
                        , currentMap
                            |> Dict.remove newLocation
                        )

                    _ ->
                        game |> Tuple.mapSecond (face location direction)
                )

            _ ->
                ( playerCell
                , game |> Tuple.mapSecond (face location direction)
                )


throwEnemy : Actor -> EnemyType -> String -> Map Cell -> Map Cell
throwEnemy (( _,direction ) as playerCell) enemyType enemyId currentMap =
    let
        newLocation : Location
        newLocation =
            playerCell |> Map.posFront 1
    in
    currentMap
        |> Dict.update newLocation (always (Just <| Stunned enemyType enemyId))
        |> (case
                currentMap
                    |> Dict.get (playerCell |> Map.posFront 2)
            of
                Just (Solid _) ->
                    identity

                Just (Enemy _ _) ->
                    identity

                _ ->
                    \newMap ->
                        newMap
                            |> Map.move ( newLocation, direction )
                            |> (case
                                    newMap
                                        |> Dict.get (playerCell |> Map.posFront 3)
                                of
                                    Just (Solid _) ->
                                        identity

                                    Just (Enemy _ _) ->
                                        identity

                                    _ ->
                                        Map.move ( playerCell |> Map.posFront 2, direction )
                               )
           )


activate : Actor -> Game -> Game
activate playerCell (( playerData, _ ) as game) =
    game
        |> (case playerData |> .inventory |> Inventory.selected of
                Just consumable ->
                    Tuple.mapFirst takeFromInventory
                        >> itemAction playerCell consumable

                Nothing ->
                    drop playerCell
           )


placingItem : Map Cell -> Actor -> Cell -> (SolidType -> Maybe (Game -> Game)) -> Maybe (Game -> Game)
placingItem map playerCell cell specialCase =
    let
        frontPos : Map.Location
        frontPos =
            playerCell |> Map.posFront 1
    in
    case map |> Dict.get frontPos of
        Nothing ->
            Just <| Tuple.mapSecond (Map.place frontPos cell)

        Just (Effect _) ->
            Just <| Tuple.mapSecond (Map.place frontPos cell)

        Just (Solid solidType) ->
            specialCase solidType

        _ ->
            Nothing


drop : Actor -> Game -> Game
drop playerCell ( playerData, map ) =
    let
        ( item, inventory ) =
            playerData.inventory |> Inventory.take

        dir : Map.Location
        dir =
            playerCell |> Map.posFront 1
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


addToInventory : ItemType -> PlayerData -> PlayerData
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


itemAction : Actor -> ItemType -> Game -> Game
itemAction playerCell consumable (( playerData, map ) as game) =
    let
        defaultCase : Game -> Game
        defaultCase =
            Tuple.mapFirst (addToInventory consumable)
    in
    game
        |> ((case consumable of
                Bombe ->
                    bombeAction map playerCell

                Material material ->
                    materialAction map playerCell material

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


bombeAction : Map Cell -> Actor -> Maybe (Game -> Game)
bombeAction currentMap playerCell =
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
                        \( playerData, map ) ->
                            ( playerData
                                |> addToInventory (Material material)
                            , map
                                |> Map.place (playerCell |> Map.posFront 1) (Solid solid)
                            )
                    )

        id : String
        id =
            let
                ( front_x, front_y ) =
                    playerCell |> Map.posFront 1
            in
            "bombe_"
                ++ String.fromInt front_x
                ++ "_"
                ++ String.fromInt front_y
    in
    placingItem currentMap playerCell (Enemy PlacedBombe id) specialCase


materialAction : Map Cell -> Actor -> MaterialType -> Maybe (Game -> Game)
materialAction map playerCell material =
    let
        specialCase : SolidType -> Maybe (Game -> Game)
        specialCase solidType =
            Just
                (case Cell.composing ( Just solidType, material ) of
                    Just newSolid ->
                        Tuple.mapSecond <| Map.place (playerCell |> Map.posFront 1) (Solid newSolid)

                    Nothing ->
                        Tuple.mapFirst <| addToInventory <| Material material
                )
    in
    Cell.composing ( Nothing, material )
        |> Maybe.andThen
            (\solid ->
                placingItem
                    map
                    playerCell
                    (Solid solid)
                    specialCase
            )
