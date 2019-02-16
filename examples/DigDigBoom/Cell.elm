module DigDigBoom.Cell exposing
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
import DigDigBoom.Tileset as Tileset
import PixelEngine.Graphics.Tile exposing (Tile)
import PixelEngine.Grid as Grid exposing (Grid)
import PixelEngine.Grid.Direction exposing (Direction(..))
import PixelEngine.Grid.Position exposing (Position)
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


tutorial : Int -> Grid Cell
tutorial num =
    Grid.fill
        (\(( x, y ) as pos) ->
            if 2 <= x && x <= 13 && 7 <= y && y <= 8 then
                case num of
                    5 ->
                        case pos of
                            ( 13, 8 ) ->
                                Just <| Enemy Rat "rat_1"

                            ( 11, 7 ) ->
                                Just <| Enemy Oger "Oger_1"

                            ( 8, 7 ) ->
                                Just <| Solid <| Placed <| Stone

                            ( 3, 8 ) ->
                                Just <| (Item <| Bombe)

                            ( 6, 7 ) ->
                                Just <| (Item <| Material <| Stone)

                            ( 7, 8 ) ->
                                Just <| (Item <| Bombe)

                            _ ->
                                Nothing

                    4 ->
                        case pos of
                            ( 9, 7 ) ->
                                Just <| Solid StoneBrickWall

                            ( 9, 8 ) ->
                                Just <| Solid StoneWall

                            ( 13, 7 ) ->
                                Just <| Enemy Goblin "goblin_1"

                            ( 7, 8 ) ->
                                Just <| (Item <| Bombe)

                            ( 8, 8 ) ->
                                Just <| (Item <| Bombe)

                            _ ->
                                Nothing

                    3 ->
                        case pos of
                            ( 10, 8 ) ->
                                Just <| Solid StoneWall

                            ( 13, 7 ) ->
                                Just <| Enemy Goblin "goblin_1"

                            ( 11, 8 ) ->
                                Just <| (Item <| Bombe)

                            _ ->
                                Nothing

                    2 ->
                        case pos of
                            ( 9, 7 ) ->
                                Just <| Solid StoneWall

                            ( 11, 7 ) ->
                                Just <| (Solid <| Placed Dirt)

                            ( 9, 8 ) ->
                                Just <| (Solid <| Placed Dirt)

                            ( 13, 7 ) ->
                                Just <| Enemy Goblin "goblin_1"

                            ( 7, 8 ) ->
                                Just <| (Item <| Bombe)

                            _ ->
                                Nothing

                    _ ->
                        case pos of
                            ( 9, 7 ) ->
                                Just <| Solid StoneWall

                            ( 13, 7 ) ->
                                Just <| Enemy Rat "rat_1"

                            ( 7, 8 ) ->
                                Just <| (Item <| Bombe)

                            _ ->
                                Nothing

            else
                Just (Solid StoneBrickWall)
        )
        { columns = 16
        , rows = 16
        }


generator : Generator (Position -> Maybe Cell)
generator =
    let
        positionToMaybeCell : Maybe Cell -> Generator (Position -> Maybe Cell)
        positionToMaybeCell maybeCell =
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
                    positionToMaybeCell <| Just <| Solid <| Placed Dirt

                else if r < 150 then
                    positionToMaybeCell <| Just <| Solid StoneWall

                else if r < 200 then
                    positionToMaybeCell <| Just <| Solid StoneBrickWall

                else if r < 225 then
                    positionToMaybeCell <| Just <| Item Bombe

                else if r < 230 then
                    positionToMaybeCell <| Just <| Item HealthPotion

                else if r < 235 then
                    Random.float 0 1
                        |> Random.andThen
                            (\id ->
                                positionToMaybeCell <| Just <| Enemy Rat <| "Rat" ++ String.fromFloat id
                            )

                else if r < 238 then
                    Random.float 0 1
                        |> Random.andThen
                            (\id ->
                                positionToMaybeCell <| Just <| Enemy Goblin <| "Goblin" ++ String.fromFloat id
                            )

                else if r < 239 then
                    Random.float 0 1
                        |> Random.andThen
                            (\id ->
                                positionToMaybeCell <| Just <| Enemy Oger <| "Oger" ++ String.fromFloat id
                            )

                else
                    positionToMaybeCell <| Nothing
            )
