module MiniWorldWar.View.Image.Card exposing (card, exit, submit, watch)

import MiniWorldWar.Data.Color as Color exposing (Color(..))
import MiniWorldWar.Data.Continent as Continent exposing (Continent(..))
import PixelEngine.Image as Image exposing (Image)
import PixelEngine.Tile as Tile exposing (Tileset)


tileset : Tileset
tileset =
    { source = "cards.png"
    , spriteWidth = 16 * 2
    , spriteHeight = 16 * 3
    }


card : Continent -> Color -> Image msg
card continent color =
    Image.fromTile
        (Tile.fromPosition
            ( Continent.toInt continent
            , color |> Color.toInt
            )
        )
        tileset


watch : Image msg
watch =
    Image.fromSrc "watch.png"


submit : Image msg
submit =
    Image.fromSrc "submit.png"


exit : Image msg
exit =
    Image.fromSrc "exit.png"
