module MiniWorldWar.Unit exposing (unitImage)

import PixelEngine.Graphics.Tile exposing (Tile, tile, Tileset, animated)
import PixelEngine.Graphics.Image as Image exposing (Image, image)
import MiniWorldWar.Board as Board exposing (Unit)
import MiniWorldWar.Color as Color exposing (Color(..))



unusedMarkerImage : Color -> Image msg
unusedMarkerImage color =
  Image.fromTile
    (tile (0,Color.toInt color) |> animated 7)
    { source = "marker.png"
    , spriteWidth = 16
    , spriteHeight = 16
    }

usedMarkerImage : Image msg
usedMarkerImage =
  Image.fromTile
    (tile (0,1))
    { source = "tileset.png"
    , spriteWidth = 16
    , spriteHeight = 16
    }

unitImage : Unit -> {used:Bool}-> Image msg
unitImage unit {used} =
  let
      {color,amount} = unit

      y = Color.toInt color
      x = if amount > 10 then
            9
          else
            amount-1
  in
  Image.multipleImages
    [ ( (0,0)
      , if used then
          usedMarkerImage
        else
          unusedMarkerImage color
      )
      ,( (0,0)
      , Image.fromTile
          (tile (x,y))
          { source = "units.png"
          , spriteWidth = 16
          , spriteHeight = 16
          }
      )
    ]