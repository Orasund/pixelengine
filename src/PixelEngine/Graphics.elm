module PixelEngine.Graphics exposing (Area, Background, Options, colorBackground, imageArea, imageBackground, options, render, tiledArea)

{-| A graphic engine for turn-based pixel games.

To get started, copy the following example:

    module TilesetExample exposing (main)

    import Css
    import Html.Styled exposing (toUnstyled)
    import PixelEngine.Graphics as Graphics exposing (Background)
    import PixelEngine.Graphics.Image exposing (image)
    import PixelEngine.Graphics.Tile as Tile
        exposing
            ( Tileset
            , tile
            )


    main =
        let
            tileSize : Int
            tileSize =
                16

            windowWidth : Int
            windowWidth =
                16

            scale : Float
            scale =
                2

            width : Float
            width =
                (toFloat <| windowWidth * tileSize) * scale

            tileset : Tileset
            tileset =
                { source = "https://orasund.github.io/pixelengine/tileset.png"
                , spriteWidth = tileSize
                , spriteHeight = tileSize
                }

            background : Background
            background =
                Graphics.colorBackground (Css.rgb 20 12 28)

            goblin =
                tile ( 2, 8 ) |> Tile.animated 1

            letter_h =
                tile ( 1, 15 )

            letter_i =
                tile ( 2, 12 )

            heart =
                tile ( 4, 8 )
        in
        Graphics.render
            { width = width
            , transitionSpeedInSec = 0.2
            , scale = scale
            }
            [ Graphics.tiledArea
                { rows = 4
                , cols = windowWidth
                , tileset = tileset
                , background = background
                }
                [ ( ( 6, 2 ), goblin )
                , ( ( 7, 2 ), letter_h )
                , ( ( 8, 2 ), letter_i )
                , ( ( 9, 2 ), heart )
                ]
            , Graphics.imageArea
                { height = scale * (toFloat <| tileSize * 12)
                , background = background
                }
                [   ( ( width / 2 - 80 * scale, 0 )
                    , image "https://orasund.github.io/pixelengine/pixelengine-logo.png"
                    )
                ]
            ]
            |> toUnstyled


## Main Function

@docs Options,options,render


## Area

@docs Area,tiledArea,imageArea


## Background

@docs Background,imageBackground,colorBackground

-}

import Css
import Html.Styled exposing (Html)
import PixelEngine.Graphics.Abstract as Abstract
import PixelEngine.Graphics.Image exposing (Image)
import PixelEngine.Graphics.Tile exposing (Tile, Tileset)


{-| A horizontal area of the content.
A area defines how the content should be displayed.

**Note:** An area can only contain elements that are supported by the type of that area.
You can find more information about the valid elements in the curresponding modules.

-}
type alias Area msg =
    Abstract.Area msg


{-| Every area has a background.
-}
type alias Background =
    Abstract.Background


{-| Options for the render function
-}
type alias Options msg =
    Abstract.Options msg


{-| A single color using the [elm-css colors](http://package.elm-lang.org/packages/rtfeldman/elm-css/latest/Css#rgb)

```
Color (Css.rgb 20 12 28)
```

-}
colorBackground : Css.Color -> Background
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
This is a complete contrast to the way how tiledArea is working.
usefull applications are GUIs, menus or loading screens.

This area has the following options:

  - height - the height or the area in pixels
  - background - the background of the area

-}
imageArea : { height : Float, background : Background } -> List ( ( Float, Float ), Image msg ) -> Area msg
imageArea { height, background } content =
    Abstract.Images
        { height = height
        , background = background
        , content = content
        }


{-| An area for using tilesets. It supports one tileset at a time,
that means all sprites must be of the same size and stored as a grid in one single file.
This area is useful for displaying the playing field of a game.

This area has the following options:

  - rows - The amount of rows of the grid. This value defines the height of the area.
  - tileset - The tileset that will be used for all elements in the area.
  - background - The background of the area.

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

  - width - Width of the game.
    **Note:** all spatial values are given in _Pixels_.

  - scale - This value scales up all content of the game.
    **Default value:** 1 for original sized images

  - transitionSpeedInSec - The speed of animations.
    **Default value:** 0 for no animations

For the start use the following settings

```
{scale = 2,width = 800, transitionSpeedInSec = 0.2}
```

-}
options : { width : Float, scale : Float, transitionSpeedInSec : Float } -> Options msg
options =
    Abstract.options


{-| This functions displays the content of the game.
It returns [elm-css Html](http://package.elm-lang.org/packages/rtfeldman/elm-css/latest/Html-Styled#Html).

The main idea of this graphic engine is to arrage the content into so called _Areas_.
These Areas are then displayed vertically on top of eachother.

-}
render : Options msg -> List (Area msg) -> Html msg
render options listOfArea =
    Abstract.render
        options
        listOfArea
