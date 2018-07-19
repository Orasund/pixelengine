module Roguelike.Cell exposing (Cell(..), ConsumableType(..), EffectType(..), EnemyType(..), Item(..), MaterialType(..), MiscellaneousType(..), SolidType, composing, decomposing, getImage, mapGenerator, resistancy)

import Dict
import PixelEngine exposing (Tile, animatedMovableTile, animatedTile, movableTile, tile)
import Random
import Roguelike.Map as Map exposing (Direction(..), Location, Map)


type ConsumableType
    = Bombe
    | Cheese
    | Material MaterialType


type MiscellaneousType
    = Bone


type EnemyType
    = PlacedBombe
    | Oger
    | Goblin
    | Rat


type EffectType
    = Smoke


type Item
    = Consumable ConsumableType
    | Miscellaneous MiscellaneousType


type Cell
    = Player Direction
    | Solid SolidType
    | Enemy EnemyType String
    | Item Item
    | Effect EffectType


type MaterialType
    = Dirt
    | Stone


type SolidType
    = DirtWall
    | StoneWall
    | StoneBrickWall
    | Placed MaterialType


decomposing : SolidType -> ( Maybe SolidType, MaterialType )
decomposing solidType =
    case solidType of
        Placed material ->
            ( Nothing, material )

        DirtWall ->
            ( Nothing, Dirt )

        StoneWall ->
            ( Just DirtWall, Stone )

        StoneBrickWall ->
            ( Just StoneWall, Stone )


composing : ( Maybe SolidType, MaterialType ) -> Maybe SolidType
composing tuple =
    case tuple of
        ( Just (Placed placedMaterial), material ) ->
            case ( placedMaterial, material ) of
                ( Dirt, Dirt ) ->
                    Just DirtWall

                ( Stone, Dirt ) ->
                    Just StoneWall

                ( Dirt, Stone ) ->
                    Just StoneWall

                ( Stone, Stone ) ->
                    Just StoneBrickWall

        ( Nothing, material ) ->
            Just (Placed material)

        ( Just solidType, _ ) ->
            Nothing


getImage : Cell -> Tile
getImage cell =
    case cell of
        Player a ->
            case a of
                Down ->
                    animatedMovableTile ( 12, 12 ) 1 "player"

                Up ->
                    animatedMovableTile ( 12, 13 ) 1 "player"

                Left ->
                    animatedMovableTile ( 12, 14 ) 1 "player"

                Right ->
                    animatedMovableTile ( 12, 15 ) 1 "player"

        Solid (Placed Stone) ->
            tile ( 0, 3 )

        Solid (Placed Dirt) ->
            tile ( 1, 1 )

        Solid StoneWall ->
            tile ( 1, 0 )

        Solid DirtWall ->
            tile ( 0, 2 )

        Solid StoneBrickWall ->
            tile ( 2, 0 )

        Item (Consumable Bombe) ->
            tile ( 6, 6 )

        Item (Consumable Cheese) ->
            tile ( 5, 7 )

        Item (Consumable (Material Dirt)) ->
            tile ( 8, 7 )

        Item (Consumable (Material Stone)) ->
            tile ( 8, 6 )

        Item (Miscellaneous Bone) ->
            tile ( 0, 10 )

        Enemy PlacedBombe _ ->
            animatedTile ( 4, 9 ) 1

        Enemy Oger id ->
            animatedMovableTile ( 0, 8 ) 1 id

        Enemy Goblin id ->
            animatedMovableTile ( 2, 8 ) 1 id

        Enemy Rat id ->
            animatedMovableTile ( 0, 9 ) 1 id

        Effect Smoke ->
            tile ( 14, 14 )



--_ ->
--   ( 7, 12 )


resistancy : SolidType -> Int
resistancy solid =
    case solid of
        DirtWall ->
            2

        StoneWall ->
            3

        StoneBrickWall ->
            4

        Placed Dirt ->
            1

        Placed Stone ->
            2


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
        ( map |> Dict.insert pos (Solid DirtWall)
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
        ( map |> Dict.insert pos (Item (Consumable Bombe))
        , new_seed
        )
    else if r < 230 then
        ( map |> Dict.insert pos (Item (Consumable Cheese))
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
    else if r < 245 then
        let
            ( id, _ ) =
                Random.step (Random.float 0 1) new_seed
        in
        ( map |> Dict.insert pos (Enemy Goblin ("Goblin" ++ toString id))
        , new_seed
        )
    else if r < 250 then
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
