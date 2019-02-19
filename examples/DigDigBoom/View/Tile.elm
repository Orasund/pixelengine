module DigDigBoom.View.Tile exposing
    ( arrow_down
    , arrow_left
    , arrow_right
    , arrow_up
    , bombe
    , bone
    , colorBlue
    , colorBrown
    , colorGray
    , colorGreen
    , colorRed
    , colorWhite
    , colorYellow
    , dirt_wall
    , goblin
    , health_potion
    , heart
    , oger
    , placed_bombe
    , placed_dirt
    , placed_stone
    , player_down
    , player_left
    , player_right
    , player_up
    , rat
    , smoke
    , stone
    , stone_brick_wall
    , stone_wall
    , stunned_bombe
    , stunned_goblin
    , stunned_oger
    , stunned_rat
    , text
    , tileset
    )

import Color exposing (Color)
import PixelEngine.Tile as Tile exposing (Tile, Tileset)


tileset : Tileset
tileset =
    Tile.tileset { source = "tileset.png", spriteHeight = 16, spriteWidth = 16 }


text : String -> Color -> ( Int, Int ) -> List ( ( Int, Int ), Tile msg )
text string color ( x, y ) =
    string
        |> Tile.fromText ( 0, 10 )
        |> List.indexedMap
            (\i tile -> ( ( x + i, y ), tile |> Tile.monochrome color ))


colorWhite : Color
colorWhite =
    Color.rgb255 222 238 214


colorGray : Color
colorGray =
    Color.rgb255 78 74 78


colorBrown : Color
colorBrown =
    Color.rgb255 133 76 48


colorYellow : Color
colorYellow =
    Color.rgb255 218 212 94


colorGreen : Color
colorGreen =
    Color.rgb255 109 170 44


colorBlue : Color
colorBlue =
    Color.rgb255 89 125 206


colorRed : Color
colorRed =
    Color.rgb255 208 70 72


arrow_up : Color -> Tile msg
arrow_up color =
    Tile.fromPosition ( 13, 9 ) |> Tile.monochrome color


arrow_down : Color -> Tile msg
arrow_down color =
    Tile.fromPosition ( 12, 9 ) |> Tile.monochrome color


arrow_right : Color -> Tile msg
arrow_right color =
    Tile.fromPosition ( 14, 9 ) |> Tile.monochrome color


arrow_left : Color -> Tile msg
arrow_left color =
    Tile.fromPosition ( 15, 9 ) |> Tile.monochrome color


player_down : Color -> Tile msg
player_down color =
    Tile.fromPosition ( 8, 8 )
        |> Tile.animated 2
        |> Tile.movable "player"
        |> Tile.monochrome color


player_up : Color -> Tile msg
player_up color =
    Tile.fromPosition ( 8, 9 )
        |> Tile.animated 2
        |> Tile.movable "player"
        |> Tile.monochrome color


player_left : Color -> Tile msg
player_left color =
    Tile.fromPosition ( 10, 8 )
        |> Tile.animated 2
        |> Tile.movable "player"
        |> Tile.monochrome color


player_right : Color -> Tile msg
player_right color =
    Tile.fromPosition ( 10, 9 )
        |> Tile.animated 2
        |> Tile.movable "player"
        |> Tile.monochrome color


placed_stone : Color -> Tile msg
placed_stone color =
    Tile.fromPosition ( 1, 1 ) |> Tile.monochrome color


placed_dirt : Color -> Tile msg
placed_dirt color =
    Tile.fromPosition ( 0, 3 ) |> Tile.monochrome color


stone_wall : Color -> Tile msg
stone_wall color =
    Tile.fromPosition ( 1, 0 ) |> Tile.monochrome color


dirt_wall : Color -> Tile msg
dirt_wall color =
    Tile.fromPosition ( 0, 2 ) |> Tile.monochrome color


stone_brick_wall : Color -> Tile msg
stone_brick_wall color =
    Tile.fromPosition ( 2, 0 ) |> Tile.monochrome color


bombe : Color -> Tile msg
bombe color =
    Tile.fromPosition ( 6, 6 ) |> Tile.monochrome color


health_potion : Color -> Tile msg
health_potion color =
    Tile.fromPosition ( 5, 7 ) |> Tile.monochrome color


stone : Color -> Tile msg
stone color =
    Tile.fromPosition ( 8, 0 ) |> Tile.monochrome color


bone : Color -> Tile msg
bone color =
    Tile.fromPosition ( 8, 6 ) |> Tile.monochrome color


placed_bombe : String -> Color -> Tile msg
placed_bombe id color =
    Tile.fromPosition ( 4, 9 )
        |> Tile.animated 2
        |> Tile.movable id
        |> Tile.monochrome color


oger : String -> Color -> Tile msg
oger id color =
    Tile.fromPosition ( 0, 8 )
        |> Tile.animated 2
        |> Tile.movable id
        |> Tile.monochrome color


goblin : String -> Color -> Tile msg
goblin id color =
    Tile.fromPosition ( 2, 8 )
        |> Tile.animated 2
        |> Tile.movable id
        |> Tile.monochrome color


rat : String -> Color -> Tile msg
rat id color =
    Tile.fromPosition ( 0, 9 )
        |> Tile.animated 2
        |> Tile.movable id
        |> Tile.monochrome color


smoke : Color -> Tile msg
smoke color =
    Tile.fromPosition ( 11, 7 ) |> Tile.monochrome color


heart : Color -> Tile msg
heart color =
    Tile.fromPosition ( 4, 8 ) |> Tile.monochrome color


stunned_oger : String -> Color -> Tile msg
stunned_oger id color =
    Tile.fromPosition ( 0, 5 )
        |> Tile.movable id
        |> Tile.monochrome color


stunned_goblin : String -> Color -> Tile msg
stunned_goblin id color =
    Tile.fromPosition ( 1, 4 )
        |> Tile.movable id
        |> Tile.monochrome color


stunned_rat : String -> Color -> Tile msg
stunned_rat id color =
    Tile.fromPosition ( 0, 4 )
        |> Tile.movable id
        |> Tile.monochrome color


stunned_bombe : String -> Color -> Tile msg
stunned_bombe id color =
    Tile.fromPosition ( 1, 5 )
        |> Tile.movable id
        |> Tile.monochrome color
