module PixelEngine.Graphics.Image exposing (Image, image, movable, fromTile, multipleImages, withAttributes)

{-| This module contains functions for creating images.
These Images can then be used for the _imageArea_ function from the main module

@docs Image, image, movable, fromTile, multipleImages, withAttributes

-}

import Html.Styled exposing (Attribute)
import PixelEngine.Graphics.Abstract as Abstract
import PixelEngine.Graphics.Tile exposing (Tile, Tileset)


{-| A image is a very general object: as we will see later,
even tiles are essentially images.
The following functions are intended to be modular.

A example of a image could be:

```
image "img.png" |> movable "uniqueName"
```

-}
type alias Image msg =
    Abstract.ContentElement msg


{-| The basic image constructor.
the string contains the url to the image

```
image "https://orasund.github.io/pixelengine/pixelengine-logo.png"
```

-}
image : String -> Image msg
image source =
    { elementType =
        Abstract.SingleSource <|
            Abstract.ImageSource source
    , customAttributes = []
    , uniqueId = Nothing
    }


{-| Makes a image transition between positions.
This is useful for images that will change their position during the game.

**Note:** The string should be unique, if not the transition might fail every now and then.

**Note:** The string will be a id Attribute in a html node, so be careful not to use names that might be already taken.

-}
movable : String -> Image msg -> Image msg
movable transitionId contentElement =
    { contentElement
        | uniqueId = Just (transitionId,True)
    }

{-| Pauses a the transition of a `movable` image.

**Only use in combination with `movable`:**
    
    image "face.png" |> movable "name" |> jumping

Use this function if a tile has the `movable`-property, but you would like to
remove it without causing any unwanted side effects.
-}
jumping : Image msg -> Image msg
jumping ({uniqueId} as t) =
    case uniqueId of
        Nothing ->
            t
        Just (id,_) ->
            {t|uniqueId = Just (id,False)}

{-| Tiles are essentially also images,
therefore this constructor transforms a tile and a tileset into an image.

```
fromTile (tile (0,0))
    (tileset {source:"https://orasund.github.io/pixelengine/pixelengine-logo.png",width:80,height:80})
==
image "https://orasund.github.io/pixelengine/pixelengine-logo.png"
```

**Note:** fromTile displays only the width and height of the image, that where given.
This means setting width and height to 0 would not display the image at all.

```
fromTile (tile (0,0) |> movable "uniqueId")
==
fromTile (tile (0,0)) |> movable "uniqueId"
```

**Note:** If you want to animate an image use this function instead

-}
fromTile : Tile msg -> Tileset -> Image msg
fromTile { info, uniqueId, customAttributes } tileset =
    let
        { top, left, steps } =
            info
    in
    { elementType =
        Abstract.SingleSource <|
            Abstract.TileSource
                { left = left
                , top = top
                , steps = steps
                , tileset = tileset
                }
    , customAttributes = customAttributes
    , uniqueId = uniqueId
    }


{-| Adds custom attributes. use the [elm-css Attributes](http://package.elm-lang.org/packages/rtfeldman/elm-css/latest/Svg-Styled-Attributes).

The motivation for this function was so that one can create [onClick](http://package.elm-lang.org/packages/rtfeldman/elm-css/latest/Html-Styled-Events#onClick) events.

-}
withAttributes : List (Attribute msg) -> Image msg -> Image msg
withAttributes attributes i =
    { i
        | customAttributes = attributes
    }


{-| it is possible to compose an image from a set of other images.
the two Floats are realtive coordinates so

```
((100,100),image "img.png")
=
((20,50), multipleimages [((80,50),image "img.png")])
```

sub-images loose the ability to be movable:

```
multipleimages [((x,y),image "img.png" |> movable "id")]
=
multipleimages [((x,y),image "img.png")]
```

instead use the following:

```
image "img.png" |> movable "id"
=
multipleimages [((0,0),image "img.png")] |> movable "id"
```

-}
multipleImages : List ( ( Float, Float ), Image msg ) -> Image msg
multipleImages list =
    let
        images : Abstract.MultipleSources
        images =
            list
                |> List.foldr
                    (\( ( left, top ), contentElement ) ->
                        case contentElement.elementType of
                            Abstract.SingleSource singleSource ->
                                (::) ( { left = left, top = top }, singleSource )

                            Abstract.MultipleSources _ ->
                                identity
                    )
                    []
    in
    { elementType =
        Abstract.MultipleSources images
    , customAttributes = []
    , uniqueId = Nothing
    }
