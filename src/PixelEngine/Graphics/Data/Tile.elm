module PixelEngine.Graphics.Data.Tile exposing (Tile, TileInformation, Tileset)

import Html.Styled exposing (Attribute)


type alias TileInformation additional =
    { additional
        | top : Int
        , left : Int
        , steps : Int
    }


type alias Tile msg =
    { info : TileInformation {}
    , customAttributes : List (Attribute msg)
    , uniqueId : Maybe ( String, Bool )
    }


type alias Tileset =
    { source : String
    , spriteWidth : Int
    , spriteHeight : Int
    }
