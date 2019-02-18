module PixelEngine.Graphics exposing
    ( Area, tiledArea, imageArea, heightOf
    , Background, imageBackground, colorBackground
    , Options, options
    , render
    , view
    )

{-| This module takes care of the Graphics.

You will want to add [PixelEngine.Graphics.Image](PixelEngine-Graphics-Image)
or [PixelEngine.Graphics.Tile](PixelEngine-Graphics-Tile) to
actually draw something.


## Area

The main idea of this graphic engine is to arrage the content into horizontal stripes,
so called areas.

@docs Area, tiledArea, imageArea, heightOf


## Background

@docs Background, imageBackground, colorBackground


# Advanced

If one wants to use just use this module on its own, you can use `view` instead
of the `game` function from the main module.


## View

@docs view

## DEPRECATED

@docs render,Options, options

-}

import Color exposing (Color)
import Html exposing (Html)
import Html.Styled
import PixelEngine.Graphics.Abstract as Abstract
import PixelEngine.Graphics.Data as Data
import PixelEngine.Graphics.Data.Area as AreaData
import PixelEngine.Graphics.Data.Options as OptionsData
import PixelEngine.Graphics.Image exposing (Image)
import PixelEngine.Graphics.Tile exposing (Tile, Tileset)


{-| A horizontal area of the content.
A `Area` defines how the content should be displayed.

**Note:**
An area can only contain elements of the same type.
So either you have tiles or images.

![A typical game](https://orasund.github.io/pixelengine/img4.png "A typical game")

-}
type alias Area msg =
    AreaData.Area msg


{-| Returns the height of a list of Areas

This can be used to return the height of a `tiledArea`.
For a `imageArea` this function is trivial.

-}
heightOf : List (Area msg) -> Float
heightOf listOfArea =
    List.sum
        (listOfArea
            |> List.map
                (\area ->
                    case area of
                        AreaData.Tiled { rows, tileset } ->
                            toFloat <| rows * tileset.spriteHeight

                        AreaData.Images { height } ->
                            height
                )
        )


{-| Every area has a background.
-}
type alias Background =
    Data.Background





{-| A single color background.
It uses [avh4/elm-color](https://package.elm-lang.org/packages/avh4/elm-color/latest).

```
colorBackground (Color.rgb255 20 12 28)
```

-}
colorBackground : Color -> Background
colorBackground color =
    Data.ColorBackground color


{-| An image that gets repeated.

```
Image "groundTile.png"
```

-}
imageBackground : { source : String, width : Float, height : Float } -> Background
imageBackground image =
    Data.ImageBackground image


{-| An area containing images that can be arranged freely.

This is a complete contrast to the way how `tiledArea` is working.
Usefull applications are GUIs, menus or loading screens.

Checkout [PixelEngine.Graphics.Image](PixelEngine-Graphics-Image) for more information.

This area has the following options:

  - `height` - The height or the `Area` in pixels.
  - `background` - The background of the `Area`.

-}
imageArea : { height : Float, background : Background } -> List ( ( Float, Float ), Image msg ) -> Area msg
imageArea { height, background } content =
    AreaData.Images
        { height = height
        , background = background
        , content = content
        }


{-| An area for using tilesets.

It supports one tileset at a time,
that means all sprites must be of the same size and stored as a grid in one single file.
This area is useful for displaying the playing field of a game.

Checkout [PixelEngine.Graphics.Tile](PixelEngine-Graphics-Image) for more information.

This area has the following options:

  - `rows` - The amount of rows of the grid. This value defines the height of the `Area`.
  - `tileset` - The tileset that will be used for all elements in the `Area`.
  - `background` - The background of the `Area`.

-}
tiledArea : { rows : Int, tileset : Tileset, background : Background } -> List ( ( Int, Int ), Tile msg ) -> Area msg
tiledArea { rows, tileset, background } content =
    AreaData.Tiled
        { rows = rows
        , tileset = tileset
        , background = background
        , content = content
        }

{-| Displays content of the game.
-}
view : Options msg -> List (Area msg) -> Html msg
view o listOfArea =
    Abstract.render
        o
        listOfArea
        |> Html.Styled.toUnstyled

----------------------------
-- DEPRECATED
----------------------------

{-| [DEPRECATED]

Use `pixelEngine.Graphics.Options.Options` instead
-}
type alias Options msg =
    OptionsData.Options msg

{-| [DEPRECATED]

Use `pixelEngine.Graphics.Options.fromWidth` instead
-}
options : { width : Float, transitionSpeedInSec : Float } -> Options msg
options { width, transitionSpeedInSec } =
    OptionsData.new
        { width = width
        , scale = 1
        , movementSpeedInSec = transitionSpeedInSec
        , animationFPS = 1
        }




{-| [DEPRECATED]

use `view` instead.
-}
render : Float -> Options msg -> List (Area msg) -> Html msg
render scale o listOfArea =
    Abstract.render
        (o |> OptionsData.usingScale (floor<|scale))
        listOfArea
        |> Html.Styled.toUnstyled
