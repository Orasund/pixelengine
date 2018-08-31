module DigDigBoom.Tileset exposing (..)

import Css
import PixelEngine.Graphics.Tile exposing (Tile, animated, backgroundColor, movable, tile, withAttributes)


colorWhite : Css.Color
colorWhite =
    Css.rgb 222 238 214


colorGray : Css.Color
colorGray =
    Css.rgb 78 74 78


colorBrown : Css.Color
colorBrown =
    Css.rgb 133 76 48


colorYellow : Css.Color
colorYellow =
    Css.rgb 218 212 94


colorGreen : Css.Color
colorGreen =
    Css.rgb 109 170 44


colorBlue : Css.Color
colorBlue =
    Css.rgb 89 125 206


colorRed : Css.Color
colorRed =
    Css.rgb 208 70 72


digit_0 : Css.Color -> Tile msg
digit_0 color =
    tile ( 8, 12 ) |> withAttributes [ backgroundColor color ]


digit_1 : Css.Color -> Tile msg
digit_1 color =
    tile ( 8, 13 ) |> withAttributes [ backgroundColor color ]


digit_2 : Css.Color -> Tile msg
digit_2 color =
    tile ( 8, 14 ) |> withAttributes [ backgroundColor color ]


digit_3 : Css.Color -> Tile msg
digit_3 color =
    tile ( 8, 15 ) |> withAttributes [ backgroundColor color ]


digit_4 : Css.Color -> Tile msg
digit_4 color =
    tile ( 9, 12 ) |> withAttributes [ backgroundColor color ]


digit_5 : Css.Color -> Tile msg
digit_5 color =
    tile ( 9, 13 ) |> withAttributes [ backgroundColor color ]


digit_6 : Css.Color -> Tile msg
digit_6 color =
    tile ( 9, 14 ) |> withAttributes [ backgroundColor color ]


digit_7 : Css.Color -> Tile msg
digit_7 color =
    tile ( 9, 15 ) |> withAttributes [ backgroundColor color ]


digit_8 : Css.Color -> Tile msg
digit_8 color =
    tile ( 10, 12 ) |> withAttributes [ backgroundColor color ]


digit_9 : Css.Color -> Tile msg
digit_9 color =
    tile ( 10, 13 ) |> withAttributes [ backgroundColor color ]


numberToTile : Int -> Css.Color -> Tile msg
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


letter_a : Css.Color -> Tile msg
letter_a color =
    tile ( 0, 12 ) |> withAttributes [ backgroundColor color ]


letter_b : Css.Color -> Tile msg
letter_b color =
    tile ( 0, 13 ) |> withAttributes [ backgroundColor color ]


letter_c : Css.Color -> Tile msg
letter_c color =
    tile ( 0, 14 ) |> withAttributes [ backgroundColor color ]


letter_d : Css.Color -> Tile msg
letter_d color =
    tile ( 0, 15 ) |> withAttributes [ backgroundColor color ]


letter_e : Css.Color -> Tile msg
letter_e color =
    tile ( 1, 12 ) |> withAttributes [ backgroundColor color ]


letter_f : Css.Color -> Tile msg
letter_f color =
    tile ( 1, 13 ) |> withAttributes [ backgroundColor color ]


letter_g : Css.Color -> Tile msg
letter_g color =
    tile ( 1, 14 ) |> withAttributes [ backgroundColor color ]


letter_h : Css.Color -> Tile msg
letter_h color =
    tile ( 1, 15 ) |> withAttributes [ backgroundColor color ]


letter_i : Css.Color -> Tile msg
letter_i color =
    tile ( 2, 12 ) |> withAttributes [ backgroundColor color ]


letter_j : Css.Color -> Tile msg
letter_j color =
    tile ( 2, 13 ) |> withAttributes [ backgroundColor color ]


letter_k : Css.Color -> Tile msg
letter_k color =
    tile ( 2, 14 ) |> withAttributes [ backgroundColor color ]


letter_l : Css.Color -> Tile msg
letter_l color =
    tile ( 2, 15 ) |> withAttributes [ backgroundColor color ]


letter_m : Css.Color -> Tile msg
letter_m color =
    tile ( 3, 12 ) |> withAttributes [ backgroundColor color ]


letter_n : Css.Color -> Tile msg
letter_n color =
    tile ( 3, 13 ) |> withAttributes [ backgroundColor color ]


letter_o : Css.Color -> Tile msg
letter_o color =
    tile ( 3, 14 ) |> withAttributes [ backgroundColor color ]


letter_p : Css.Color -> Tile msg
letter_p color =
    tile ( 3, 15 ) |> withAttributes [ backgroundColor color ]


letter_q : Css.Color -> Tile msg
letter_q color =
    tile ( 4, 12 ) |> withAttributes [ backgroundColor color ]


letter_r : Css.Color -> Tile msg
letter_r color =
    tile ( 4, 13 ) |> withAttributes [ backgroundColor color ]


letter_s : Css.Color -> Tile msg
letter_s color =
    tile ( 4, 14 ) |> withAttributes [ backgroundColor color ]


letter_t : Css.Color -> Tile msg
letter_t color =
    tile ( 4, 15 ) |> withAttributes [ backgroundColor color ]


letter_u : Css.Color -> Tile msg
letter_u color =
    tile ( 5, 12 ) |> withAttributes [ backgroundColor color ]


letter_v : Css.Color -> Tile msg
letter_v color =
    tile ( 5, 13 ) |> withAttributes [ backgroundColor color ]


letter_w : Css.Color -> Tile msg
letter_w color =
    tile ( 5, 14 ) |> withAttributes [ backgroundColor color ]


letter_x : Css.Color -> Tile msg
letter_x color =
    tile ( 5, 15 ) |> withAttributes [ backgroundColor color ]


letter_y : Css.Color -> Tile msg
letter_y color =
    tile ( 6, 12 ) |> withAttributes [ backgroundColor color ]


letter_z : Css.Color -> Tile msg
letter_z color =
    tile ( 6, 13 ) |> withAttributes [ backgroundColor color ]


letter_period : Css.Color -> Tile msg
letter_period color =
    tile ( 6, 14 ) |> withAttributes [ backgroundColor color ]


letter_comma : Css.Color -> Tile msg
letter_comma color =
    tile ( 6, 15 ) |> withAttributes [ backgroundColor color ]


letter_question_mark : Css.Color -> Tile msg
letter_question_mark color =
    tile ( 7, 12 ) |> withAttributes [ backgroundColor color ]


letter_exclamation_mark : Css.Color -> Tile msg
letter_exclamation_mark color =
    tile ( 7, 13 ) |> withAttributes [ backgroundColor color ]


letter_colon : Css.Color -> Tile msg
letter_colon color =
    tile ( 7, 14 ) |> withAttributes [ backgroundColor color ]


letter_plus : Css.Color -> Tile msg
letter_plus color =
    tile ( 10, 14 ) |> withAttributes [ backgroundColor color ]


letter_minus : Css.Color -> Tile msg
letter_minus color =
    tile ( 10, 15 ) |> withAttributes [ backgroundColor color ]


letter_equals : Css.Color -> Tile msg
letter_equals color =
    tile ( 7, 15 ) |> withAttributes [ backgroundColor color ]


arrow_up : Css.Color -> Tile msg
arrow_up color =
    tile ( 11, 12 ) |> withAttributes [ backgroundColor color ]


arrow_down : Css.Color -> Tile msg
arrow_down color =
    tile ( 11, 13 ) |> withAttributes [ backgroundColor color ]


arrow_right : Css.Color -> Tile msg
arrow_right color =
    tile ( 11, 14 ) |> withAttributes [ backgroundColor color ]


arrow_left : Css.Color -> Tile msg
arrow_left color =
    tile ( 11, 15 ) |> withAttributes [ backgroundColor color ]


player_down : Css.Color -> Tile msg
player_down color =
    tile ( 12, 12 ) |> animated 1 |> movable "player" |> withAttributes [ backgroundColor color ]


player_up : Css.Color -> Tile msg
player_up color =
    tile ( 12, 13 ) |> animated 1 |> movable "player" |> withAttributes [ backgroundColor color ]


player_left : Css.Color -> Tile msg
player_left color =
    tile ( 12, 14 ) |> animated 1 |> movable "player" |> withAttributes [ backgroundColor color ]


player_right : Css.Color -> Tile msg
player_right color =
    tile ( 12, 15 ) |> animated 1 |> movable "player" |> withAttributes [ backgroundColor color ]


placed_stone : Css.Color -> Tile msg
placed_stone color =
    tile ( 1, 1 ) |> withAttributes [ backgroundColor color ]


placed_dirt : Css.Color -> Tile msg
placed_dirt color =
    tile ( 0, 3 ) |> withAttributes [ backgroundColor color ]


stone_wall : Css.Color -> Tile msg
stone_wall color =
    tile ( 1, 0 ) |> withAttributes [ backgroundColor color ]


dirt_wall : Css.Color -> Tile msg
dirt_wall color =
    tile ( 0, 2 ) |> withAttributes [ backgroundColor color ]


stone_brick_wall : Css.Color -> Tile msg
stone_brick_wall color =
    tile ( 2, 0 ) |> withAttributes [ backgroundColor color ]


bombe : Css.Color -> Tile msg
bombe color =
    tile ( 6, 6 ) |> withAttributes [ backgroundColor color ]


health_potion : Css.Color -> Tile msg
health_potion color =
    tile ( 5, 7 ) |> withAttributes [ backgroundColor color ]


dirt : Css.Color -> Tile msg
dirt color =
    tile ( 8, 7 ) |> withAttributes [ backgroundColor color ]


stone : Css.Color -> Tile msg
stone color =
    tile ( 8, 6 ) |> withAttributes [ backgroundColor color ]


bone : Css.Color -> Tile msg
bone color =
    tile ( 0, 10 ) |> withAttributes [ backgroundColor color ]


placed_bombe : String -> Css.Color -> Tile msg
placed_bombe id color =
    tile ( 4, 9 ) |> animated 1 |> movable id |> withAttributes [ backgroundColor color ]


oger : String -> Css.Color -> Tile msg
oger id color =
    tile ( 0, 8 ) |> animated 1 |> movable id |> withAttributes [ backgroundColor color ]


goblin : String -> Css.Color -> Tile msg
goblin id color =
    tile ( 2, 8 ) |> animated 1 |> movable id |> withAttributes [ backgroundColor color ]


rat : String -> Css.Color -> Tile msg
rat id color =
    tile ( 0, 9 ) |> animated 1 |> movable id |> withAttributes [ backgroundColor color ]


smoke : Css.Color -> Tile msg
smoke color =
    tile ( 14, 14 ) |> withAttributes [ backgroundColor color ]


heart : Css.Color -> Tile msg
heart color =
    tile ( 4, 8 ) |> withAttributes [ backgroundColor color ]


stunned_oger : String -> Css.Color -> Tile msg
stunned_oger id color =
    tile ( 0, 5 ) |> movable id |> withAttributes [ backgroundColor color ]


stunned_goblin : String -> Css.Color -> Tile msg
stunned_goblin id color =
    tile ( 1, 4 ) |> movable id |> withAttributes [ backgroundColor color ]


stunned_rat : String -> Css.Color -> Tile msg
stunned_rat id color =
    tile ( 0, 4 ) |> movable id |> withAttributes [ backgroundColor color ]


stunned_bombe : String -> Css.Color -> Tile msg
stunned_bombe id color =
    tile ( 1, 5 ) |> movable id |> withAttributes [ backgroundColor color ]
