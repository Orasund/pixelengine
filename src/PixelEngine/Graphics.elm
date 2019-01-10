module PixelEngine.Graphics exposing
    ( Options, options, render
    , Area, tiledArea, imageArea, heightOf
    , Background, imageBackground, colorBackground
    , usingScale
    )
    
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

            width : Float
            width =
                toFloat <| windowWidth * tileSize

            tileset : Tileset
            tileset =
                { source = "https://orasund.github.io/pixelengine/DigDigBoom/tileset.png"
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
            (Graphics.options
                { width = width
                , transitionSpeedInSec = 0.2
                }
                |> Graphics.usingScale 2
            )
            [ Graphics.tiledArea
                { rows = 4
                , tileset = tileset
                , background = background
                }
                [ ( ( 6, 2 ), goblin |> Tile.withAttributes [ Tile.backgroundColor (Css.rgb 170 57 57) ] )
                , ( ( 7, 2 ), letter_h |> Tile.withAttributes [ Tile.backgroundColor (Css.rgb 97 81 146) ] )
                , ( ( 8, 2 ), letter_i |> Tile.withAttributes [ Tile.backgroundColor (Css.rgb 170 151 57) ] )
                , ( ( 9, 2 ), heart |> Tile.withAttributes [ Tile.backgroundColor (Css.rgb 45 134 51) ] )
                ]
            , Graphics.imageArea
                { height = toFloat <| tileSize * 12
                , background = background
                }
                [ ( ( width / 2 - 80, 0 )
                , image "https://orasund.github.io/pixelengine/pixelengine-logo.png"
                )
                ]
            ]


## Main Function

@docs Options, options, render


## Area

@docs Area, tiledArea, imageArea, heightOf


## Background

@docs Background, imageBackground, colorBackground


## Advanced

@docs usingScale

-}

import Color exposing (Color)
import Html exposing (Html)
import Html.Styled
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



{-| returns the height of a list of Areas
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


{-| A single color using the [elm-css colors](http://package.elm-lang.org/packages/rtfeldman/elm-css/latest/Css#rgb)

```
Color (Css.rgb 20 12 28)
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

  - transitionSpeedInSec - The speed of animations.
    **Default value:** 0 for no animations

For the start use the following settings

```
{width = 800, transitionSpeedInSec = 0.2}
```

-}
options : { width : Float, transitionSpeedInSec : Float } -> Options msg
options { width, transitionSpeedInSec } =
    Abstract.newOptions { width = width, scale = 1, transitionSpeedInSec = transitionSpeedInSec }


{-| scale up EVERYTHING.
it can not be used with PixelEngine.program.
-}
usingScale : Float -> Options msg -> Options msg
usingScale scale (Abstract.Options o) =
    Abstract.Options { o | scale = scale }


{-| This functions displays the content of the game.
It returns [elm-css Html](http://package.elm-lang.org/packages/rtfeldman/elm-css/latest/Html-Styled#Html).

The main idea of this graphic engine is to arrage the content into so called _Areas_.
These Areas are then displayed vertically on top of eachother.

-}
render : Options msg -> List (Area msg) -> Html msg
render o listOfArea =
    Abstract.render
        o
        listOfArea
    |> Html.Styled.toUnstyled