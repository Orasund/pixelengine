module PixelEngine.Graphics.Tile exposing
    ( Tile, tile, movable, animated
    , withAttributes, onClick, backgroundColor
    , Tileset, tileset
    , jumping
    )

{-| This module contains functions for creating tiles.
Tiles are used for the `tiledArea` function from the main module.


## Tile

@docs Tile, tile, movable, jumping ,animated


## Attributes

@docs withAttributes, onClick, backgroundColor


## Tileset

@docs Tileset, tileset

-}

import Html exposing (Attribute)
import Html.Attributes as Attributes
import Html.Events as Events
import Html.Styled.Attributes
import PixelEngine.Graphics.Abstract as Abstract
import Color exposing (Color)


{-| A Tileset contains the actuall image that a `Tile` can reference.
-}
type alias Tileset =
    Abstract.Tileset


{-| The `Tileset` constructor has the following parameters:

  - `source` - The adress to the file
  - `spriteWidth` - The size of a single sprite in the `Tileset` (in pixels)
  - `spriteHeight` - The height of a single sprite (also in pixels)

For the following defines the `Tileset` used in the examples of this module.

    { width: 16
    , height 16
    , source: "https://orasund.github.io/pixelengine/tileset.png"
    }

-}
tileset : { source : String, spriteWidth : Int, spriteHeight : Int } -> Tileset
tileset =
    identity


{-| A `Tile` defines a sprite in a `Tileset`.
The following functions are intended to be modular.

A example for a `tile` could be:

    tile ( 1, 2 ) |> animated 1 |> movable "uniqueName"

-}
type alias Tile msg =
    Abstract.Tile msg


{-| The basic `Tile` constructor.

The first argument is the position of the sprite in the `tileset`.

As an example

    tile ( 3, 2 )

is the 3 row in the second column of the `Tileset`.
![a tileset](https://orasund.github.io/pixelengine/img3.png "a tileset")
-}
tile : ( Int, Int ) -> Tile msg
tile ( left, top ) =
    { info = { top = top, left = left, steps = 0 }
    , uniqueId = Nothing
    , customAttributes = []
    }


{-| Adds animations to a `Tile`.
The sprites of the animation must be arranged horizontally in the `Tileset`.

The first argument give the amount of steps of the animation (one less then the number of sprites.)

The following code specifies a `Tile` with 3+1 frames

    tile ( 0, 0 ) |> animated 3

**Note:**  
Setting the steps to `0` describes a tile with no animation.

    tile ( 0, 0 ) |> animated 0 == tile ( 0, 0 )

![animation](https://orasund.github.io/pixelengine/img2.png "animation")

**Note:**  
Negaive steps are not supported, in this case no animation will be played.

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


{-| Makes a `Tile` transition between positions.
This is useful for sprites that will change their position during the game.

    tile ( 0, 0 ) |> movable "name"

**Note:**  
Once a `Tile` has this property, it can **NOT** be removed during the game.

**Note:**  
The string should be unique,. If not then the transition might fail every now and then.

**Note:**  
The string will be a id Attribute in a html node, so be careful not to use names that might be already taken.

-}
movable : String -> Tile msg -> Tile msg
movable id t =
    { t
        | uniqueId = Just ( id, True )
    }


{-| Pauses the transition of a `movable` tile.

**Only use in combination with `movable`:**

    tile ( 0, 0 ) |> movable "name" |> jumping

Use this function if a `Tile` has the `movable`-property and you would like to
remove it temporarily without causing any unwanted side effects.
-}
jumping : Tile msg -> Tile msg
jumping ({ uniqueId } as t) =
    case uniqueId of
        Nothing ->
            t

        Just ( id, _ ) ->
            { t | uniqueId = Just ( id, False ) }


{-| Adds custom attributes.

use the [Html.Attributes](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes).

Use this to create the `onClick` event from [Html.Events](https://package.elm-lang.org/packages/elm/html/latest/Html-Events#onClick).
-}
withAttributes : List (Attribute msg) -> Tile msg -> Tile msg
withAttributes attributes ({ customAttributes } as t) =
    { t
        | customAttributes =
            attributes
            |> List.map Html.Styled.Attributes.fromUnstyled
            |> List.append customAttributes 
    }


{-| Adds a background color.

This can be used to simulate monochrome sprites or to implement team colors.

    withAttributes [css [Css.backgroundColor <| Css.rgb 255 0 0]]
    =
    withAttributes [ backgroundColor <| Color.rgb255 255 0 0]

-}
backgroundColor : Color -> Attribute msg
backgroundColor =
    Color.toCssString
    >> Attributes.style "background-color"


{-| returns a Msg when it has been clicked.

    withAttributes [Events.onClick msg]
    =
    withAttributes [ onClick msg]

-}
onClick : msg -> Attribute msg
onClick msg =
    Events.onClick msg
