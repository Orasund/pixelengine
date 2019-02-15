module PixelEngine.Graphics.Data.Area exposing (Area(..), ContentElement, ImageAreaContent, TiledAreaContent)

import Html.Styled exposing (Attribute)
import PixelEngine.Graphics.Data exposing (Background, Location)
import PixelEngine.Graphics.Data.Element exposing (Element)
import PixelEngine.Graphics.Data.Tile exposing (Tile, Tileset)


type alias ContentElement msg =
    { elementType : Element
    , customAttributes : List (Attribute msg)
    , uniqueId : Maybe ( String, Bool )
    }


type alias ImageAreaContent msg =
    { height : Float
    , background : Background
    , content : List ( ( Float, Float ), ContentElement msg )
    }


type alias TiledAreaContent msg =
    { rows : Int
    , tileset : Tileset
    , background : Background
    , content : List ( Location, Tile msg )
    }


type Area msg
    = Tiled (TiledAreaContent msg)
    | Images (ImageAreaContent msg)
