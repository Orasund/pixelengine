module DigDigBoom.Tileset exposing (..)

import PixelEngine.Graphics.Tile exposing (Tile, animated, movable, tile)


digit_0 : Tile msg
digit_0 =
    tile ( 8, 12 )


digit_1 : Tile msg
digit_1 =
    tile ( 8, 13 )


digit_2 : Tile msg
digit_2 =
    tile ( 8, 14 )


digit_3 : Tile msg
digit_3 =
    tile ( 8, 15 )


digit_4 : Tile msg
digit_4 =
    tile ( 9, 12 )


digit_5 : Tile msg
digit_5 =
    tile ( 9, 13 )


digit_6 : Tile msg
digit_6 =
    tile ( 9, 14 )


digit_7 : Tile msg
digit_7 =
    tile ( 9, 15 )


digit_8 : Tile msg
digit_8 =
    tile ( 10, 12 )


digit_9 : Tile msg
digit_9 =
    tile ( 10, 13 )


numberToTile : Int -> Tile msg
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


letter_a : Tile msg
letter_a =
    tile ( 0, 12 )


letter_b : Tile msg
letter_b =
    tile ( 0, 13 )


letter_c : Tile msg
letter_c =
    tile ( 0, 14 )


letter_d : Tile msg
letter_d =
    tile ( 0, 15 )


letter_e : Tile msg
letter_e =
    tile ( 1, 12 )


letter_f : Tile msg
letter_f =
    tile ( 1, 13 )


letter_g : Tile msg
letter_g =
    tile ( 1, 14 )


letter_h : Tile msg
letter_h =
    tile ( 1, 15 )


letter_i : Tile msg
letter_i =
    tile ( 2, 12 )


letter_j : Tile msg
letter_j =
    tile ( 2, 13 )


letter_k : Tile msg
letter_k =
    tile ( 2, 14 )


letter_l : Tile msg
letter_l =
    tile ( 2, 15 )


letter_m : Tile msg
letter_m =
    tile ( 3, 12 )


letter_n : Tile msg
letter_n =
    tile ( 3, 13 )


letter_o : Tile msg
letter_o =
    tile ( 3, 14 )


letter_p : Tile msg
letter_p =
    tile ( 3, 15 )


letter_q : Tile msg
letter_q =
    tile ( 4, 12 )


letter_r : Tile msg
letter_r =
    tile ( 4, 13 )


letter_s : Tile msg
letter_s =
    tile ( 4, 14 )


letter_t : Tile msg
letter_t =
    tile ( 4, 15 )


letter_u : Tile msg
letter_u =
    tile ( 5, 12 )


letter_v : Tile msg
letter_v =
    tile ( 5, 13 )


letter_w : Tile msg
letter_w =
    tile ( 5, 14 )


letter_x : Tile msg
letter_x =
    tile ( 5, 15 )


letter_y : Tile msg
letter_y =
    tile ( 6, 12 )


letter_z : Tile msg
letter_z =
    tile ( 6, 13 )


letter_period : Tile msg
letter_period =
    tile ( 6, 14 )


letter_comma : Tile msg
letter_comma =
    tile ( 6, 15 )


letter_question_mark : Tile msg
letter_question_mark =
    tile ( 7, 12 )


letter_exclamation_mark : Tile msg
letter_exclamation_mark =
    tile ( 7, 13 )


letter_colon : Tile msg
letter_colon =
    tile ( 7, 14 )


letter_plus : Tile msg
letter_plus =
    tile ( 10, 14 )


letter_minus : Tile msg
letter_minus =
    tile ( 10, 15 )


letter_equals : Tile msg
letter_equals =
    tile ( 7, 15 )


arrow_up : Tile msg
arrow_up =
    tile ( 11, 12 )


arrow_down : Tile msg
arrow_down =
    tile ( 11, 13 )


arrow_right : Tile msg
arrow_right =
    tile ( 11, 14 )


arrow_left : Tile msg
arrow_left =
    tile ( 11, 15 )


player_down : Tile msg
player_down =
    tile ( 12, 12 ) |> animated 1 |> movable "player"


player_up : Tile msg
player_up =
    tile ( 12, 13 ) |> animated 1 |> movable "player"


player_left : Tile msg
player_left =
    tile ( 12, 14 ) |> animated 1 |> movable "player"


player_right : Tile msg
player_right =
    tile ( 12, 15 ) |> animated 1 |> movable "player"


placed_stone : Tile msg
placed_stone =
    tile ( 1, 1 )


placed_dirt : Tile msg
placed_dirt =
    tile ( 0, 3 )


stone_wall : Tile msg
stone_wall =
    tile ( 1, 0 )


dirt_wall : Tile msg
dirt_wall =
    tile ( 0, 2 )


stone_brick_wall : Tile msg
stone_brick_wall =
    tile ( 2, 0 )


bombe : Tile msg
bombe =
    tile ( 6, 6 )


health_potion : Tile msg
health_potion =
    tile ( 5, 7 )


dirt : Tile msg
dirt =
    tile ( 8, 7 )


stone : Tile msg
stone =
    tile ( 8, 6 )


bone : Tile msg
bone =
    tile ( 0, 10 )


placed_bombe : String -> Tile msg
placed_bombe id =
    tile ( 4, 9 ) |> animated 1 |> movable id


oger : String -> Tile msg
oger id =
    tile ( 0, 8 ) |> animated 1 |> movable id


goblin : String -> Tile msg
goblin id =
    tile ( 2, 8 ) |> animated 1 |> movable id


rat : String -> Tile msg
rat id =
    tile ( 0, 9 ) |> animated 1 |> movable id


smoke : Tile msg
smoke =
    tile ( 14, 14 )


heart : Tile msg
heart =
    tile ( 4, 8 )


stunned_oger : String -> Tile msg
stunned_oger id =
    tile ( 0, 5 ) |> movable id


stunned_goblin : String -> Tile msg
stunned_goblin id =
    tile ( 1, 4 ) |> movable id


stunned_rat : String -> Tile msg
stunned_rat id =
    tile ( 0, 4 ) |> movable id


stunned_bombe : String -> Tile msg
stunned_bombe id =
    tile ( 1, 5 ) |> movable id
