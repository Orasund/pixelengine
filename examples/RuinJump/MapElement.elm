module RuinJump.MapElement exposing (
        Block(..), MapElement(..),
        isOccupied, woodGenerator, remove,dirtGenerator, toTiles
    )

import PixelEngine.Graphics.Tile exposing (Tile)
import Random exposing (Generator)
import RuinJump.Player exposing (FaceingDirection(..),  PlayerAction(..))
import RuinJump.Tileset as Tileset
import PixelEngine.Component.Position exposing( Position)

type Block
    = Dirt
    | Grass
    | Stone
    | Wood
    | Air

type MapElement
    = PlayerElement PlayerAction FaceingDirection
    | BlockElement Block Int

isOccupied : Maybe MapElement -> Bool
isOccupied elem =
    case elem of
        Nothing ->
            False

        Just (BlockElement Air _) ->
            False

        Just _ ->
            True

remove : MapElement -> MapElement
remove element =
    case element of
                PlayerElement _ _ ->
                    BlockElement Air 0

                BlockElement _ id ->
                    BlockElement Air id

dirtGenerator : Generator MapElement
dirtGenerator =
    Random.int 0 Random.maxInt |> Random.map (BlockElement Dirt)

woodGenerator : Generator MapElement
woodGenerator =
    Random.int 0 Random.maxInt |> Random.map (BlockElement Wood)


toTiles : Position -> MapElement -> List ( Position, Tile msg )
toTiles pos mapElement =
    case mapElement of
        PlayerElement action faceingDirection ->
            case action of
                Standing ->
                    pos
                        |> (case faceingDirection of
                                FaceingLeft ->
                                    Tileset.player_left

                                FaceingRight ->
                                    Tileset.player_right
                           )

                Falling ->
                    pos
                        |> (case faceingDirection of
                                FaceingLeft ->
                                    Tileset.player_jump_left

                                FaceingRight ->
                                    Tileset.player_jump_right
                           )

        BlockElement block id ->
            List.singleton
                ( pos
                , id
                    |> case block of
                        Dirt ->
                            Tileset.dirt
                        
                        Grass ->
                            Tileset.grass
                        
                        Stone ->
                            Tileset.stone
                        
                        Wood ->
                            Tileset.wood
                        
                        Air ->
                            Tileset.air
                )

