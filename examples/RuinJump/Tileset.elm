module RuinJump.Tileset exposing
    ( dirt
    , grass
    , player_jump_left
    , player_jump_right
    , player_left
    , player_right
    , stone
    , wood
    , air
    )

import Natural exposing (Natural16(..))
import PixelEngine.Graphics.Tile exposing (Tile, movable, tile)


variantTile : Int -> ( Int, Int ) -> Tile msg
variantTile int ( x, y ) =
    let
        variant : Int
        variant =
            int |> modBy 16
    in
    tile ( x + (variant |> modBy 4), y + variant // 4 )


quadCoordinates : ( Int, Int ) -> List ( Int, Int )
quadCoordinates ( x, y ) =
    [ ( x, y ), ( x + 1, y ), ( x, y + 1 ), ( x + 1, y + 1 ) ]


quadTile : { tilePos : ( Int, Int ), pos : ( Int, Int ) } -> List ( ( Int, Int ), Tile msg )
quadTile { tilePos, pos } =
    List.map2
        (\p t -> ( p, t ))
        (quadCoordinates pos)
        (quadCoordinates tilePos |> List.map tile)

air : Int -> Tile msg
air _ =
    tile (1,0)
        {- |> movable ""-}


dirt : Int -> Tile msg
dirt seed =
    variantTile seed ( 4, 0 )
        {- |> movable ""-}

wood : Int -> Tile msg
wood seed =
    variantTile seed ( 8, 0 )
        {- |> movable ""-}

{- ("tile_" ++ (String.fromInt seed)) -}


stone : Int -> Tile msg
stone seed =
    variantTile seed ( 0, 4 )
        {- |> movable ""-}



{- ("tile_" ++ (String.fromInt seed)) -}


grass : Int -> Tile msg
grass seed =
    variantTile seed ( 4, 4 )
        {- |> movable ""-}



{- ("tile_" ++ (String.fromInt seed)) -}


player_left : ( Int, Int ) -> List ( ( Int, Int ), Tile msg )
player_left pos =
    quadTile { tilePos = ( 0, 8 ), pos = pos }
        |> List.indexedMap (\i -> Tuple.mapSecond <| movable <| "player" ++ String.fromInt i)


player_right : ( Int, Int ) -> List ( ( Int, Int ), Tile msg )
player_right pos =
    quadTile { tilePos = ( 2, 8 ), pos = pos }
        |> List.indexedMap (\i -> Tuple.mapSecond <| movable <| "player" ++ String.fromInt i)


player_jump_left : ( Int, Int ) -> List ( ( Int, Int ), Tile msg )
player_jump_left pos =
    quadTile { tilePos = ( 0, 10 ), pos = pos }
        |> List.indexedMap (\i -> Tuple.mapSecond <| movable <| "player" ++ String.fromInt i)


player_jump_right : ( Int, Int ) -> List ( ( Int, Int ), Tile msg )
player_jump_right pos =
    quadTile { tilePos = ( 2, 10 ), pos = pos }
        |> List.indexedMap (\i -> Tuple.mapSecond <| movable <| "player" ++ String.fromInt i)
