module MiniWorldWar.Card exposing (card)

import PixelEngine.Graphics.Image as Image exposing (Image, image)
import PixelEngine.Graphics.Tile exposing (Tile, tile, Tileset)
import MiniWorldWar.Color as Color exposing (Color(..))
import MiniWorldWar.Continent exposing (Continent(..))

tileset : Tileset
tileset = 
  { source = "cards.png"
  , spriteWidth = 16*2
  , spriteHeight = 16*3
  }

continentToInt : Continent -> Int
continentToInt continent =
  case continent of
    SouthAmerica -> 0
    NorthAmerica -> 1
    Asia -> 2
    Europe -> 3
    Africa -> 4

card : Continent -> Color -> Image msg
card continent color =
  Image.fromTile
    (tile
      ( continentToInt continent
      , color |> Color.toInt
      )
    )
    tileset