module AsteroidMiner.View.Inventory exposing (view)

import AsteroidMiner.Data exposing (size, spriteSize)
import AsteroidMiner.Data.Item exposing (Item(..))
import AsteroidMiner.View.Map as Map
import AsteroidMiner.View.Tileset as Tileset
import Location exposing (Location)
import PixelEngine.Image as Image exposing (Image)
import PixelEngine.Tile as Tile


viewAmount : { amount : Int, i : Int } -> ( Location, Image msg )
viewAmount { amount, i } =
    let
        base10 : Int
        base10 =
            amount |> abs |> toFloat |> logBase 10 |> floor

        loc : Int -> Location
        loc offset =
            ( spriteSize * (toFloat <| size - offset - 2), i |> toFloat )

        sign : Int
        sign =
            if amount < 0 then
                1

            else
                0

        ( location, text ) =
            if base10 < 3 then
                ( loc <| base10 + sign, amount |> String.fromInt )

            else if base10 < 6 then
                ( loc <| base10 - 3 + 1 + sign, (amount // 10 ^ 3 |> String.fromInt) ++ "k" )

            else if base10 < 9 then
                ( loc <| base10 - 6 + 1 + sign, (amount // 10 ^ 6 |> String.fromInt) ++ "M" )

            else if base10 < 12 then
                ( loc <| base10 - 9 + 1 + sign, (amount // 10 ^ 9 |> String.fromInt) ++ "G" )

            else if base10 < 15 then
                ( loc <| base10 - 12 + 1 + sign, (amount // 10 ^ 12 |> String.fromInt) ++ "T" )

            else if base10 < 18 then
                ( loc <| base10 - 15 + 1 + sign, (amount // 10 ^ 15 |> String.fromInt) ++ "P" )

            else if base10 < 21 then
                ( loc <| base10 - 18 + 1 + sign, (amount // 10 ^ 18 |> String.fromInt) ++ "E" )

            else if base10 < 24 then
                ( loc <| base10 - 21 + 1 + sign, (amount // 10 ^ 21 |> String.fromInt) ++ "Z" )

            else
                ( loc <| base10 - 24 + 1 + sign, (amount // 10 ^ 24 |> String.fromInt) ++ "Y" )
    in
    ( location
    , Image.fromText text Tileset.font
    )


view : Int -> List ( Location, Image msg )
view amount =
    if amount > 0 then
        [ ( ( spriteSize * (toFloat <| size - 1), 0 )
          , Tileset.tileset
                |> Image.fromTile
                    (Tile.multipleTiles
                        [ Tileset.itemBackground, Map.viewItem Stone ]
                    )
          )
        , viewAmount { amount = amount, i = 0 }
        ]

    else
        []
