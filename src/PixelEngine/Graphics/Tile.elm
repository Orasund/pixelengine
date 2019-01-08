module PixelEngine.Graphics.Tile exposing
    ( Tile, tile, movable, animated
    , withAttributes, onClick, backgroundColor
    , Tileset, tileset
    )

{-| This module contains functions for creating tiles.
These tiles are used for the _tiledArea_ function from the main module.


## Tile

@docs Tile, tile, movable, animated


## Attributes

@docs withAttributes, onClick, backgroundColor


## Tileset

@docs Tileset, tileset

-}

import Css
import Html.Styled exposing (Attribute)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events as Events
import PixelEngine.Graphics.Abstract as Abstract


{-| A Tileset contains the actuall image that a tile can reference.
-}
type alias Tileset =
    Abstract.Tileset


{-| The tileset constructor has the following parameters:

  - source - The adress to the file
  - spriteWidth - The size of a single sprite in the tileset (in pixels)
  - spriteHeight - The height of a single sprite (also in pixels)

For example the following defines the tileset that is used in the example.

    {width: 16, height 16,source: "https://orasund.github.io/pixelengine/tileset.png"}

-}
tileset : { source : String, spriteWidth : Int, spriteHeight : Int } -> Tileset
tileset =
    identity


{-| A tile defines a sprite in a tileset.
The following functions are intended to be modular.

A example for a tile could be:

    tile ( 1, 2 ) |> animated 1 |> movable "uniqueName"

-}
type alias Tile msg =
    Abstract.Tile msg


{-| The basic tile constructor.
The two arguments is the position of the sprite in the tileset.

As an example

    tile ( 4, 2 )

is the 4 row in the second column of the tileset.

-}
tile : ( Int, Int ) -> Tile msg
tile ( left, top ) =
    { info = { top = top, left = left, steps = 0 }
    , uniqueId = Nothing
    , customAttributes = []
    }


{-| Adds animations to a tile.
The sprites of the animation must be arranged horizontally in the tileset.

  - steps - Steps of the animation (one less then the number of sprites.)

The following code specifies a tile with 3+1 frames

    tile ( 0, 0 ) |> animated 3

**Note:** Setting the steps to 0 describes a tile with no animation.

    tile ( 0, 0 ) |> animated 0 == tile ( 0, 0 )

**Note:** Negaive steps are not supported, in this case no animation will be played.

-}
animated : Int -> Tile msg -> Tile msg
animated steps ({ info } as t) =
    { t
        | info =
            { info
                | steps =
                    if steps > 0 then
                        steps

                    else
                        0
            }
    }


{-| Makes a Tile transition between positions.
This is useful for sprites that will change their position during the game.

**Note:** Once a Tile has this property, it can **NOT** be removed during the game.

**Note:** The string should be unique, if not the transition might fail every now and then.

**Note:** The string will be a id Attribute in a html node, so be careful not to use names that might be already taken.

-}
movable : String -> Tile msg -> Tile msg
movable id t =
    { t
        | uniqueId = Just ( id, True )
    }

{-| Pauses a the transition of a `movable` tile.

**Only use in combination with `movable`:**
    
    tile ( 0, 0 ) |> movable "name" |> jumping

Use this function if a tile has the `movable`-property, but you would like to
remove it without causing any unwanted side effects.
-}
jumping : Tile msg -> Tile msg
jumping ({uniqueId} as t) =
    case uniqueId of
        Nothing ->
            t
        Just (id,_) ->
            {t|uniqueId = Just (id,False)}

{-| Adds custom attributes. use the [elm-css Attributes](http://package.elm-lang.org/packages/rtfeldman/elm-css/latest/Html-Styled-Attributes).
-}
withAttributes : List (Attribute msg) -> Tile msg -> Tile msg
withAttributes attributes ({ customAttributes } as t) =
    { t
        | customAttributes = List.append customAttributes attributes
    }


{-| Adds a background color.

This can be used to simulate monochrome sprites or to implement team colors.

    withAttributes [css [Css.backgroundColor <| Css.rgb 255 0 0]]
    =
    withAttributes [ backgroundColor <| Css.rgb 255 0 0]

-}
backgroundColor : Css.Color -> Attribute msg
backgroundColor color =
    css [ Css.backgroundColor <| color ]


{-| returns a Msg when it has been clicked.

    withAttributes [Events.onClick msg]
    =
    withAttributes [ onClick msg]

-}
onClick : msg -> Attribute msg
onClick msg =
    Events.onClick msg
