module DigDigBoom.Cell
    exposing
        ( Cell(..)
        , EffectType(..)
        , EnemyType(..)
        , ItemType(..)
        , MaterialType(..)
        , SolidType
        , composing
        , decomposing
        , getImage
        , mapGenerator
        , resistancy
        , tutorial
        )

import Dict
import DigDigBoom.Component.Map exposing (Direction(..), Location, Map)
import DigDigBoom.Tileset as Tileset
import PixelEngine.Graphics.Tile exposing (Tile)
import Random


type ItemType
    = Bombe
    | HealthPotion
    | Material MaterialType


type EnemyType
    = PlacedBombe
    | Oger
    | Goblin
    | Rat


type EffectType
    = Smoke
    | Bone


type Cell
    = Player Direction
    | Solid SolidType
    | Enemy EnemyType String
    | Stunned EnemyType String
    | Item ItemType
    | Effect EffectType


type MaterialType
    = Dirt
    | Stone


type SolidType
    = StoneWall
    | StoneBrickWall
    | Placed MaterialType


decomposing : SolidType -> ( Maybe SolidType, MaterialType )
decomposing solidType =
    case solidType of
        Placed material ->
            ( Nothing, material )

        StoneWall ->
            ( Just (Placed Dirt), Stone )

        StoneBrickWall ->
            ( Just StoneWall, Stone )


composing : ( Maybe SolidType, MaterialType ) -> Maybe SolidType
composing tuple =
    case tuple of
        ( Just (Placed placedMaterial), material ) ->
            case ( placedMaterial, material ) of
                ( Dirt, Dirt ) ->
                    Nothing

                ( Stone, Dirt ) ->
                    Just StoneWall

                ( Dirt, Stone ) ->
                    Just StoneWall

                ( Stone, Stone ) ->
                    Just StoneBrickWall

        ( Nothing, material ) ->
            Just (Placed material)

        ( Just _, _ ) ->
            Nothing


getImage : Cell -> Tile msg
getImage cell =
    case cell of
        Player a ->
            case a of
                Down ->
                    Tileset.player_down

                Up ->
                    Tileset.player_up

                Left ->
                    Tileset.player_left

                Right ->
                    Tileset.player_right

        Solid (Placed Stone) ->
            Tileset.placed_stone

        Solid (Placed Dirt) ->
            Tileset.dirt_wall

        Solid StoneWall ->
            Tileset.stone_wall

        Solid StoneBrickWall ->
            Tileset.stone_brick_wall

        Item Bombe ->
            Tileset.bombe

        Item HealthPotion ->
            Tileset.health_potion

        Item (Material Dirt) ->
            Tileset.dirt

        Item (Material Stone) ->
            Tileset.stone

        Enemy PlacedBombe id ->
            Tileset.placed_bombe id

        Enemy Oger id ->
            Tileset.oger id

        Enemy Goblin id ->
            Tileset.goblin id

        Enemy Rat id ->
            Tileset.rat id

        Stunned PlacedBombe id ->
            Tileset.stunned_bombe id

        Stunned Oger id ->
            Tileset.stunned_oger id

        Stunned Goblin id ->
            Tileset.stunned_goblin id

        Stunned Rat id ->
            Tileset.stunned_rat id

        Effect Smoke ->
            Tileset.smoke

        Effect Bone ->
            Tileset.bone



--_ ->
--   ( 7, 12 )


resistancy : SolidType -> Int
resistancy solid =
    case solid of
        StoneWall ->
            3

        StoneBrickWall ->
            4

        Placed Dirt ->
            2

        Placed Stone ->
            2


tutorial : Int -> Map Cell
tutorial num =
    let
        allBrick : Map Cell
        allBrick =
            List.range 0 15
                |> List.foldl
                    (\x out ->
                        List.range 0 15
                            |> List.foldl
                                (\y map ->
                                    map |> Dict.update ( x, y ) (always (Just (Solid StoneBrickWall)))
                                )
                                out
                    )
                    Dict.empty

        newTutorial : Map Cell
        newTutorial =
            List.range 2 13
                |> List.foldl
                    (\x out ->
                        List.range 7 8
                            |> List.foldl
                                (\y map ->
                                    map |> Dict.update ( x, y ) (always Nothing)
                                )
                                out
                    )
                    allBrick
    in
    case num of
        5 ->
            newTutorial
                |> Dict.update ( 13, 8 ) (always <| Just <| Enemy Rat "rat_1")
                |> Dict.update ( 11, 7 ) (always <| Just <| Enemy Oger "Oger_1")
                |> Dict.update ( 8, 7 ) (always <| Just <| Solid <| Placed <| Stone)
                |> Dict.update ( 3, 8 ) (always <| Just <| Item <| Bombe)
                |> Dict.update ( 6, 7 ) (always <| Just <| Item <| Material <| Stone)
                |> Dict.update ( 7, 8 ) (always <| Just <| Item <| Bombe)

        4 ->
            newTutorial
                |> Dict.update ( 9, 7 ) (always <| Just <| Solid StoneBrickWall)
                |> Dict.update ( 9, 8 ) (always <| Just <| Solid StoneWall)
                |> Dict.update ( 13, 7 ) (always <| Just <| Enemy Goblin "goblin_1")
                |> Dict.update ( 7, 8 ) (always <| Just <| Item <| Bombe)
                |> Dict.update ( 8, 8 ) (always <| Just <| Item <| Bombe)

        3 ->
            newTutorial
                |> Dict.update ( 10, 8 ) (always <| Just <| Solid StoneWall)
                |> Dict.update ( 13, 7 ) (always <| Just <| Enemy Goblin "goblin_1")
                |> Dict.update ( 11, 8 ) (always <| Just <| Item <| Bombe)

        2 ->
            newTutorial
                |> Dict.update ( 9, 7 ) (always <| Just <| Solid StoneWall)
                |> Dict.update ( 11, 7 ) (always <| Just <| Solid <| Placed Dirt)
                |> Dict.update ( 9, 8 ) (always <| Just <| Solid <| Placed Dirt)
                |> Dict.update ( 13, 7 ) (always <| Just <| Enemy Goblin "goblin_1")
                |> Dict.update ( 7, 8 ) (always <| Just <| Item <| Bombe)

        _ ->
            newTutorial
                |> Dict.update ( 9, 7 ) (always <| Just <| Solid StoneWall)
                |> Dict.update ( 13, 7 ) (always <| Just <| Enemy Rat "rat_1")
                |> Dict.update ( 7, 8 ) (always <| Just <| Item <| Bombe)


mapGenerator : Location -> ( Map Cell, Random.Seed ) -> ( Map Cell, Random.Seed )
mapGenerator pos ( map, seed ) =
    let
        ( r, new_seed ) =
            Random.step (Random.int 0 500) seed
    in
    if
        (pos |> Tuple.first)
            == 0
            || (pos |> Tuple.second)
            == 0
            || (pos |> Tuple.first)
            == 15
            || (pos |> Tuple.second)
            == 15
    then
        ( map |> Dict.insert pos (Solid StoneBrickWall)
        , seed
        )
    else if r < 50 then
        ( map |> Dict.insert pos (Solid (Placed Dirt))
        , new_seed
        )
    else if r < 150 then
        ( map |> Dict.insert pos (Solid StoneWall)
        , new_seed
        )
    else if r < 200 then
        ( map |> Dict.insert pos (Solid StoneBrickWall)
        , new_seed
        )
    else if r < 225 then
        ( map |> Dict.insert pos (Item Bombe)
        , new_seed
        )
    else if r < 230 then
        ( map |> Dict.insert pos (Item HealthPotion)
        , new_seed
        )
    else if r < 235 then
        let
            ( id, _ ) =
                Random.step (Random.float 0 1) new_seed
        in
        ( map |> Dict.insert pos (Enemy Rat ("Rat" ++ toString id))
        , new_seed
        )
    else if r < 238 then
        let
            ( id, _ ) =
                Random.step (Random.float 0 1) new_seed
        in
        ( map |> Dict.insert pos (Enemy Goblin ("Goblin" ++ toString id))
        , new_seed
        )
    else if r < 239 then
        let
            ( id, _ ) =
                Random.step (Random.float 0 1) new_seed
        in
        ( map |> Dict.insert pos (Enemy Oger ("Oger" ++ toString id))
        , new_seed
        )
    else
        ( map
        , new_seed
        )
