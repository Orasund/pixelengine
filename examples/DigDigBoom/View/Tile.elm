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
import PixelEngine.Graphics.Tile as Tile exposing (Tile, animated, backgroundColor, movable, tile, withAttributes,Tileset)

tileset : Tileset
tileset =
    Tile.tileset { source = "tileset.png", spriteHeight = 16, spriteWidth = 16 }


text : String -> Color -> ( Int, Int ) -> List ( ( Int, Int ), Tile msg )
text string color ( x, y ) =
    string
        |> Tile.fromText ( 0, 10 )
        |> List.indexedMap (\i tile -> ( ( x + i, y ), tile |> withAttributes [ backgroundColor color ] ))


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
    tile ( 13, 9 ) |> withAttributes [ backgroundColor color ]


arrow_down : Color -> Tile msg
arrow_down color =
    tile ( 12, 9 ) |> withAttributes [ backgroundColor color ]


arrow_right : Color -> Tile msg
arrow_right color =
    tile ( 14, 9 ) |> withAttributes [ backgroundColor color ]


arrow_left : Color -> Tile msg
arrow_left color =
    tile ( 15, 9 ) |> withAttributes [ backgroundColor color ]


player_down : Color -> Tile msg
player_down color =
    tile ( 8, 8 ) |> animated 1 |> movable "player" |> withAttributes [ backgroundColor color ]


player_up : Color -> Tile msg
player_up color =
    tile ( 8, 9 ) |> animated 1 |> movable "player" |> withAttributes [ backgroundColor color ]


player_left : Color -> Tile msg
player_left color =
    tile ( 10, 8 ) |> animated 1 |> movable "player" |> withAttributes [ backgroundColor color ]


player_right : Color -> Tile msg
player_right color =
    tile ( 10, 9 ) |> animated 1 |> movable "player" |> withAttributes [ backgroundColor color ]


placed_stone : Color -> Tile msg
placed_stone color =
    tile ( 1, 1 ) |> withAttributes [ backgroundColor color ]


placed_dirt : Color -> Tile msg
placed_dirt color =
    tile ( 0, 3 ) |> withAttributes [ backgroundColor color ]


stone_wall : Color -> Tile msg
stone_wall color =
    tile ( 1, 0 ) |> withAttributes [ backgroundColor color ]


dirt_wall : Color -> Tile msg
dirt_wall color =
    tile ( 0, 2 ) |> withAttributes [ backgroundColor color ]


stone_brick_wall : Color -> Tile msg
stone_brick_wall color =
    tile ( 2, 0 ) |> withAttributes [ backgroundColor color ]


bombe : Color -> Tile msg
bombe color =
    tile ( 6, 6 ) |> withAttributes [ backgroundColor color ]


health_potion : Color -> Tile msg
health_potion color =
    tile ( 5, 7 ) |> withAttributes [ backgroundColor color ]


stone : Color -> Tile msg
stone color =
    tile ( 8, 0 ) |> withAttributes [ backgroundColor color ]


bone : Color -> Tile msg
bone color =
    tile ( 8, 6 ) |> withAttributes [ backgroundColor color ]


placed_bombe : String -> Color -> Tile msg
placed_bombe id color =
    tile ( 4, 9 ) |> animated 1 |> movable id |> withAttributes [ backgroundColor color ]


oger : String -> Color -> Tile msg
oger id color =
    tile ( 0, 8 ) |> animated 1 |> movable id |> withAttributes [ backgroundColor color ]


goblin : String -> Color -> Tile msg
goblin id color =
    tile ( 2, 8 ) |> animated 1 |> movable id |> withAttributes [ backgroundColor color ]


rat : String -> Color -> Tile msg
rat id color =
    tile ( 0, 9 ) |> animated 1 |> movable id |> withAttributes [ backgroundColor color ]


smoke : Color -> Tile msg
smoke color =
    tile ( 11, 7 ) |> withAttributes [ backgroundColor color ]


heart : Color -> Tile msg
heart color =
    tile ( 4, 8 ) |> withAttributes [ backgroundColor color ]


stunned_oger : String -> Color -> Tile msg
stunned_oger id color =
    tile ( 0, 5 ) |> movable id |> withAttributes [ backgroundColor color ]


stunned_goblin : String -> Color -> Tile msg
stunned_goblin id color =
    tile ( 1, 4 ) |> movable id |> withAttributes [ backgroundColor color ]


stunned_rat : String -> Color -> Tile msg
stunned_rat id color =
    tile ( 0, 4 ) |> movable id |> withAttributes [ backgroundColor color ]


stunned_bombe : String -> Color -> Tile msg
stunned_bombe id color =
    tile ( 1, 5 ) |> movable id |> withAttributes [ backgroundColor color ]
