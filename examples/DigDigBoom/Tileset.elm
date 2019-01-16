module DigDigBoom.Tileset exposing
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
    , letter_a
    , letter_b
    , letter_c
    , letter_colon
    , letter_comma
    , letter_d
    , letter_e
    , letter_equals
    , letter_exclamation_mark
    , letter_f
    , letter_g
    , letter_h
    , letter_i
    , letter_j
    , letter_k
    , letter_l
    , letter_m
    , letter_minus
    , letter_n
    , letter_o
    , letter_p
    , letter_period
    , letter_plus
    , letter_q
    , letter_question_mark
    , letter_r
    , letter_s
    , letter_t
    , letter_u
    , letter_v
    , letter_w
    , letter_x
    , letter_y
    , letter_z
    , numberToTile
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
    )

import Color exposing (Color)
import PixelEngine.Graphics.Tile exposing (Tile, animated, backgroundColor, movable, tile, withAttributes)


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


digit_0 : Color -> Tile msg
digit_0 color =
    tile ( 8, 12 ) |> withAttributes [ backgroundColor color ]


digit_1 : Color -> Tile msg
digit_1 color =
    tile ( 8, 13 ) |> withAttributes [ backgroundColor color ]


digit_2 : Color -> Tile msg
digit_2 color =
    tile ( 8, 14 ) |> withAttributes [ backgroundColor color ]


digit_3 : Color -> Tile msg
digit_3 color =
    tile ( 8, 15 ) |> withAttributes [ backgroundColor color ]


digit_4 : Color -> Tile msg
digit_4 color =
    tile ( 9, 12 ) |> withAttributes [ backgroundColor color ]


digit_5 : Color -> Tile msg
digit_5 color =
    tile ( 9, 13 ) |> withAttributes [ backgroundColor color ]


digit_6 : Color -> Tile msg
digit_6 color =
    tile ( 9, 14 ) |> withAttributes [ backgroundColor color ]


digit_7 : Color -> Tile msg
digit_7 color =
    tile ( 9, 15 ) |> withAttributes [ backgroundColor color ]


digit_8 : Color -> Tile msg
digit_8 color =
    tile ( 10, 12 ) |> withAttributes [ backgroundColor color ]


digit_9 : Color -> Tile msg
digit_9 color =
    tile ( 10, 13 ) |> withAttributes [ backgroundColor color ]


numberToTile : Int -> Color -> Tile msg
numberToTile digit =
    case digit of
        1 ->
            digit_1

        2 ->
            digit_2

        3 ->
            digit_3

        4 ->
            digit_4

        5 ->
            digit_5

        6 ->
            digit_6

        7 ->
            digit_7

        8 ->
            digit_8

        9 ->
            digit_9

        _ ->
            digit_0


letter_a : Color -> Tile msg
letter_a color =
    tile ( 0, 12 ) |> withAttributes [ backgroundColor color ]


letter_b : Color -> Tile msg
letter_b color =
    tile ( 0, 13 ) |> withAttributes [ backgroundColor color ]


letter_c : Color -> Tile msg
letter_c color =
    tile ( 0, 14 ) |> withAttributes [ backgroundColor color ]


letter_d : Color -> Tile msg
letter_d color =
    tile ( 0, 15 ) |> withAttributes [ backgroundColor color ]


letter_e : Color -> Tile msg
letter_e color =
    tile ( 1, 12 ) |> withAttributes [ backgroundColor color ]


letter_f : Color -> Tile msg
letter_f color =
    tile ( 1, 13 ) |> withAttributes [ backgroundColor color ]


letter_g : Color -> Tile msg
letter_g color =
    tile ( 1, 14 ) |> withAttributes [ backgroundColor color ]


letter_h : Color -> Tile msg
letter_h color =
    tile ( 1, 15 ) |> withAttributes [ backgroundColor color ]


letter_i : Color -> Tile msg
letter_i color =
    tile ( 2, 12 ) |> withAttributes [ backgroundColor color ]


letter_j : Color -> Tile msg
letter_j color =
    tile ( 2, 13 ) |> withAttributes [ backgroundColor color ]


letter_k : Color -> Tile msg
letter_k color =
    tile ( 2, 14 ) |> withAttributes [ backgroundColor color ]


letter_l : Color -> Tile msg
letter_l color =
    tile ( 2, 15 ) |> withAttributes [ backgroundColor color ]


letter_m : Color -> Tile msg
letter_m color =
    tile ( 3, 12 ) |> withAttributes [ backgroundColor color ]


letter_n : Color -> Tile msg
letter_n color =
    tile ( 3, 13 ) |> withAttributes [ backgroundColor color ]


letter_o : Color -> Tile msg
letter_o color =
    tile ( 3, 14 ) |> withAttributes [ backgroundColor color ]


letter_p : Color -> Tile msg
letter_p color =
    tile ( 3, 15 ) |> withAttributes [ backgroundColor color ]


letter_q : Color -> Tile msg
letter_q color =
    tile ( 4, 12 ) |> withAttributes [ backgroundColor color ]


letter_r : Color -> Tile msg
letter_r color =
    tile ( 4, 13 ) |> withAttributes [ backgroundColor color ]


letter_s : Color -> Tile msg
letter_s color =
    tile ( 4, 14 ) |> withAttributes [ backgroundColor color ]


letter_t : Color -> Tile msg
letter_t color =
    tile ( 4, 15 ) |> withAttributes [ backgroundColor color ]


letter_u : Color -> Tile msg
letter_u color =
    tile ( 5, 12 ) |> withAttributes [ backgroundColor color ]


letter_v : Color -> Tile msg
letter_v color =
    tile ( 5, 13 ) |> withAttributes [ backgroundColor color ]


letter_w : Color -> Tile msg
letter_w color =
    tile ( 5, 14 ) |> withAttributes [ backgroundColor color ]


letter_x : Color -> Tile msg
letter_x color =
    tile ( 5, 15 ) |> withAttributes [ backgroundColor color ]


letter_y : Color -> Tile msg
letter_y color =
    tile ( 6, 12 ) |> withAttributes [ backgroundColor color ]


letter_z : Color -> Tile msg
letter_z color =
    tile ( 6, 13 ) |> withAttributes [ backgroundColor color ]


letter_period : Color -> Tile msg
letter_period color =
    tile ( 6, 14 ) |> withAttributes [ backgroundColor color ]


letter_comma : Color -> Tile msg
letter_comma color =
    tile ( 6, 15 ) |> withAttributes [ backgroundColor color ]


letter_question_mark : Color -> Tile msg
letter_question_mark color =
    tile ( 7, 12 ) |> withAttributes [ backgroundColor color ]


letter_exclamation_mark : Color -> Tile msg
letter_exclamation_mark color =
    tile ( 7, 13 ) |> withAttributes [ backgroundColor color ]


letter_colon : Color -> Tile msg
letter_colon color =
    tile ( 7, 14 ) |> withAttributes [ backgroundColor color ]


letter_plus : Color -> Tile msg
letter_plus color =
    tile ( 10, 14 ) |> withAttributes [ backgroundColor color ]


letter_minus : Color -> Tile msg
letter_minus color =
    tile ( 10, 15 ) |> withAttributes [ backgroundColor color ]


letter_equals : Color -> Tile msg
letter_equals color =
    tile ( 7, 15 ) |> withAttributes [ backgroundColor color ]


arrow_up : Color -> Tile msg
arrow_up color =
    tile ( 11, 12 ) |> withAttributes [ backgroundColor color ]


arrow_down : Color -> Tile msg
arrow_down color =
    tile ( 11, 13 ) |> withAttributes [ backgroundColor color ]


arrow_right : Color -> Tile msg
arrow_right color =
    tile ( 11, 14 ) |> withAttributes [ backgroundColor color ]


arrow_left : Color -> Tile msg
arrow_left color =
    tile ( 11, 15 ) |> withAttributes [ backgroundColor color ]


player_down : Color -> Tile msg
player_down color =
    tile ( 12, 12 ) |> animated 1 |> movable "player" |> withAttributes [ backgroundColor color ]


player_up : Color -> Tile msg
player_up color =
    tile ( 12, 13 ) |> animated 1 |> movable "player" |> withAttributes [ backgroundColor color ]


player_left : Color -> Tile msg
player_left color =
    tile ( 12, 14 ) |> animated 1 |> movable "player" |> withAttributes [ backgroundColor color ]


player_right : Color -> Tile msg
player_right color =
    tile ( 12, 15 ) |> animated 1 |> movable "player" |> withAttributes [ backgroundColor color ]


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
    tile ( 8, 6 ) |> withAttributes [ backgroundColor color ]


bone : Color -> Tile msg
bone color =
    tile ( 0, 10 ) |> withAttributes [ backgroundColor color ]


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
    tile ( 14, 14 ) |> withAttributes [ backgroundColor color ]


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
