module PixelEngine.Graphics.Data.Element exposing
    ( Element(..)
    , MultipleSources
    , SingleImage
    , SingleSource(..)
    , TileWithTileset
    )

import PixelEngine.Graphics.Data.Tile exposing (Tileset,TileInformation)
import PixelEngine.Graphics.Data exposing( Position)

--------------------
--  MultipleSources
--------------------


type alias MultipleSources =
    List ( Position, SingleSource )



--------------------
-- SingleSource
--------------------


type alias SingleImage =
    String


type alias TileWithTileset =
    TileInformation { tileset : Tileset }


type SingleSource
    = TileSource TileWithTileset
    | ImageSource SingleImage



--------------------
-- ElementType
--------------------


type Element
    = SingleSource SingleSource
    | MultipleSources MultipleSources
