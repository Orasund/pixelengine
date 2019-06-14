module PixelEngine.Graphics.Data.Tile exposing (Tile, TileInformation, Tileset, mapTile)

import Html.Styled exposing (Attribute)
import Html.Styled.Attributes as Attributes


type alias TileInformation additional =
    { additional
        | top : Int
        , left : Int
        , steps : Int
    }


type alias Tile msg =
    { info : List (TileInformation {})
    , customAttributes : List (Attribute msg)
    , uniqueId : Maybe ( String, Bool )
    }


type alias Tileset =
    { source : String
    , spriteWidth : Int
    , spriteHeight : Int
    }


mapTile : (a -> b) -> Tile a -> Tile b
mapTile fun ({ info, customAttributes, uniqueId } as elem) =
    { info = info
    , customAttributes = customAttributes |> List.map (Attributes.map fun)
    , uniqueId = uniqueId
    }
