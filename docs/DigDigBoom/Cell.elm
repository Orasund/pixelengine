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
        , generator
        , getImage
        , resistancy
        , tutorial
        )

import Dict
import DigDigBoom.Component.Map exposing (Direction(..), Location, Map)
import DigDigBoom.Tileset as Tileset
import PixelEngine.Graphics.Tile exposing (Tile)
import Random exposing (Generator)


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
            (case a of
                Down ->
                    Tileset.player_down

                Up ->
                    Tileset.player_up

                Left ->
                    Tileset.player_left

                Right ->
                    Tileset.player_right
            )
                Tileset.colorWhite

        Solid (Placed Stone) ->
            Tileset.placed_stone Tileset.colorGray

        Solid (Placed Dirt) ->
            Tileset.dirt_wall Tileset.colorBrown

        Solid StoneWall ->
            Tileset.stone_wall Tileset.colorGray

        Solid StoneBrickWall ->
            Tileset.stone_brick_wall Tileset.colorGray

        Item Bombe ->
            Tileset.bombe Tileset.colorGreen

        Item HealthPotion ->
            Tileset.health_potion Tileset.colorGreen

        Item (Material Dirt) ->
            Tileset.dirt_wall Tileset.colorBlue

        Item (Material Stone) ->
            Tileset.stone Tileset.colorBlue

        Enemy enemy id ->
            (case enemy of
                PlacedBombe ->
                    Tileset.placed_bombe id

                Oger ->
                    Tileset.oger id

                Goblin ->
                    Tileset.goblin id

                Rat ->
                    Tileset.rat id
            )
                Tileset.colorRed

        Stunned enemy id ->
            (case enemy of
                PlacedBombe ->
                    Tileset.placed_bombe id

                Oger ->
                    Tileset.oger id

                Goblin ->
                    Tileset.goblin id

                Rat ->
                    Tileset.rat id
            )
                Tileset.colorYellow

        Effect effect ->
            (case effect of
                Smoke ->
                    Tileset.smoke

                Bone ->
                    Tileset.bone
            )
                Tileset.colorWhite



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


generator : Generator (Location -> Maybe Cell)
generator =
    let
        locationToMaybeCell : Maybe Cell -> Generator (Location -> Maybe Cell)
        locationToMaybeCell maybeCell =
            constant
                (\pos ->
                    case pos of
                        ( 0, _ ) ->
                            mapBorder

                        ( 15, _ ) ->
                            mapBorder

                        ( _, 0 ) ->
                            mapBorder

                        ( _, 15 ) ->
                            mapBorder

                        _ ->
                            maybeCell
                )

        mapBorder : Maybe Cell
        mapBorder =
            Just <| Solid StoneBrickWall

        constant : a -> Generator a
        constant a =
            Random.int 0 0 |> Random.map (always a)
    in
    Random.int 0 500
        |> Random.andThen
            (\r ->
                if r < 50 then
                    locationToMaybeCell <| Just <| Solid <| Placed Dirt
                else if r < 150 then
                    locationToMaybeCell <| Just <| Solid StoneWall
                else if r < 200 then
                    locationToMaybeCell <| Just <| Solid StoneBrickWall
                else if r < 225 then
                    locationToMaybeCell <| Just <| Item Bombe
                else if r < 230 then
                    locationToMaybeCell <| Just <| Item HealthPotion
                else if r < 235 then
                    Random.float 0 1
                        |> Random.andThen
                            (\id ->
                                locationToMaybeCell <| Just <| Enemy Rat <| "Rat" ++ toString id
                            )
                else if r < 238 then
                    Random.float 0 1
                        |> Random.andThen
                            (\id ->
                                locationToMaybeCell <| Just <| Enemy Goblin <| "Goblin" ++ toString id
                            )
                else if r < 239 then
                    Random.float 0 1
                        |> Random.andThen
                            (\id ->
                                locationToMaybeCell <| Just <| Enemy Oger <| "Oger" ++ toString id
                            )
                else
                    locationToMaybeCell <| Nothing
            )
