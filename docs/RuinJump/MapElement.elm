module RuinJump.MapElement exposing (Block(..), MapElement(..), woodGenerator, remove,dirtGenerator, toTiles)

import PixelEngine.Graphics.Tile exposing (Tile)
import Random exposing (Generator)
import RuinJump.Player exposing (FaceingDirection(..),  PlayerAction(..))
import RuinJump.Tileset as Tileset


type Block
    = Dirt
    | Grass
    | Stone
    | Wood
    | Air


type MapElement
    = PlayerElement PlayerAction FaceingDirection
    | BlockElement Block Int

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


toTiles : ( Int, Int ) -> MapElement -> List ( ( Int, Int ), Tile msg )
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

