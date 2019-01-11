module PixelEngine.Graphics exposing
    ( Options, options, render
    , Area, tiledArea, imageArea, heightOf
    , Background, imageBackground, colorBackground
    )
    
{-| This module takes care of the Graphics.

You will want to add [PixelEngine.Graphics.Image](/PixelEngine-Graphics-Image)
or [PixelEngine.Graphics.Tile](/PixelEngine-Graphics-Tile) to
actually draw something.

## Area

The main idea of this graphic engine is to arrage the content into horizontal stripes,
so called areas.

@docs Area, tiledArea, imageArea, heightOf

## Background

@docs Background, imageBackground, colorBackground

## Options

@docs Options, options

# Advanced

If one wants to use just use this module on its own, you can use `render` instead
of the `game` function from the main module.

## Render

@docs render

-}

import Color exposing (Color)
import Html exposing (Html)
import Html.Styled
import PixelEngine.Graphics.Abstract as Abstract
import PixelEngine.Graphics.Image exposing (Image)
import PixelEngine.Graphics.Tile exposing (Tile, Tileset)


{-| A horizontal area of the content.
A `Area` defines how the content should be displayed.

**Note:** An area can only contain elements of the same type.
So either you have tiles or images.
-}
type alias Area msg =
    Abstract.Area msg



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
                        Abstract.Tiled { rows, tileset } ->
                            toFloat <| rows * tileset.spriteHeight

                        Abstract.Images { height } ->
                            height
                )
        )


{-| Every area has a background.
-}
type alias Background =
    Abstract.Background


{-| Options for the render function
-}
type alias Options msg =
    Abstract.Options msg


{-| A single color background.
It uses [avh4/elm-color](https://package.elm-lang.org/packages/avh4/elm-color/latest).

```
colorBackground (Color.rgb255 20 12 28)
```

-}
colorBackground : Color -> Background
colorBackground color =
    Abstract.ColorBackground color


{-| An image that gets repeated.

```
Image "groundTile.png"
```

-}
imageBackground : { source : String, width : Float, height : Float } -> Background
imageBackground image =
    Abstract.ImageBackground image


{-| An area containing images that can be arranged freely.

This is a complete contrast to the way how `tiledArea` is working.
Usefull applications are GUIs, menus or loading screens.

Checkout [PixelEngine.Graphics.Image](/PixelEngine-Graphics-Image) for more information.

This area has the following options:

  - `height` - The height or the `Area` in pixels.
  - `background` - The background of the `Area`.

-}
imageArea : { height : Float, background : Background } -> List ( ( Float, Float ), Image msg ) -> Area msg
imageArea { height, background } content =
    Abstract.Images
        { height = height
        , background = background
        , content = content
        }


{-| An area for using tilesets.

It supports one tileset at a time,
that means all sprites must be of the same size and stored as a grid in one single file.
This area is useful for displaying the playing field of a game.

Checkout [PixelEngine.Graphics.Tile](/PixelEngine-Graphics-Image) for more information.

This area has the following options:

  - `rows` - The amount of rows of the grid. This value defines the height of the `Area`.
  - `tileset` - The tileset that will be used for all elements in the `Area`.
  - `background` - The background of the `Area`.

-}
tiledArea : { rows : Int, tileset : Tileset, background : Background } -> List ( ( Int, Int ), Tile msg ) -> Area msg
tiledArea { rows, tileset, background } content =
    Abstract.Tiled
        { rows = rows
        , tileset = tileset
        , background = background
        , content = content
        }


{-| The engine comes with a set of options:

  - `width` - Width of the game.  
    **Note:** all spatial values are given in _Pixels_.

  - `transitionSpeedInSec` - The speed of animations.  
    **Default value:** `0` for no animations

For the start use the following settings

```
{width = 800, transitionSpeedInSec = 0.2}
```

-}
options : { width : Float, transitionSpeedInSec : Float } -> Options msg
options { width, transitionSpeedInSec } =
    Abstract.newOptions { width = width, scale = 1, transitionSpeedInSec = transitionSpeedInSec }


{-| Displays content of the game.

**Note:**  
The first argument is the scale. Use only power of `2` as scale to ensure crisp
pixels.
-}
render : Float -> Options msg -> List (Area msg) -> Html msg
render scale o listOfArea =
    Abstract.render
        (o |> Abstract.usingScale scale)
        listOfArea
    |> Html.Styled.toUnstyled