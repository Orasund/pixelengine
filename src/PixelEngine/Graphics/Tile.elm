module PixelEngine.Graphics.Tile exposing (Tile, Tileset, animated, movable, tile, tileset)

{-| a

@docs Tile, animated, movable, tile


## Tileset

@docs Tileset,tileset

-}

import PixelEngine.Graphics.Abstract as Abstract


{-| A tileset. It contains the link to the image as well as the size of a tile.

```
{source: "tileset.png",width: 16, height 16}
```

-}
type alias Tileset =
    Abstract.Tileset


{-| tileset
-}
tileset : { source : String, spriteWidth : Int, spriteHeight : Int } -> Tileset
tileset =
    identity


{-| a Tiles
-}
type alias Tile msg =
    Abstract.Tile msg


{-| A basic tile.
The first tile in a tileset is obtailed by

```
tile (0,0)
```

-}
tile : ( Int, Int ) -> Tile msg
tile ( left, top ) =
    { info = { top = top, left = left, steps = 0 }
    , uniqueId = Nothing
    , customAttributes = []
    }


{-| An animated tile.
The sprites of the animation must be arranged horizontally in the tileset.

  - steps - Steps of the animation (one less then the number of sprites.)

```
animatedTile (0,0) 0 == tile (0,0)
```

-}
animated : Int -> Tile msg -> Tile msg
animated steps ({ info } as tile) =
    { tile
        | info =
            { info
                | steps =
                    if steps > 0 then
                        steps
                    else
                        0
            }
    }


{-| A movable tile.
This means it will transition if the location gets chanced.

**Note:** the id should be a unique string, if not the transition might fail every now and then.

-}
movable : String -> Tile msg -> Tile msg
movable id tile =
    { tile
        | uniqueId = Just id
    }



{- s
 -}
{- withAttributes : List (Attribute msg) -> Tile msg -> Tile msg
   withAttributes attributes image =
       { image
           | customAttributes = attributes
       }
-}
