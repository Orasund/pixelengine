module PixelEngine.Graphics.Image exposing
    ( Image, image, movable, jumping, fromTile, multipleImages, clickable, monochrome, withAttributes
    , onClick
    )

{-| This module contains functions for creating images.
These Images can then be used for the `imageArea` function from the <PixelEngine>


## Image

@docs Image, image, movable, jumping, fromTile, multipleImages, clickable, monochrome, withAttributes


## DEPRECATED

@docs onClick

-}

import Color exposing (Color)
import Html exposing (Attribute)
import Html.Attributes as Attributes
import Html.Events as Events
import Html.Styled.Attributes
import PixelEngine.Graphics.Abstract as Abstract
import PixelEngine.Graphics.Tile exposing ( Tile, Tileset)


{-| A `Image` is actually a very general type: As we will see later,
even tiles are essentially images.
The following functions are intended to be modular.
-}
type alias Image msg =
    Abstract.ContentElement msg


{-| The basic image constructor.
The string contains the url to the image

    image "aStone.png"

-}
image : String -> Image msg
image source =
    { elementType =
        Abstract.SingleSource <|
            Abstract.ImageSource source
    , customAttributes = []
    , uniqueId = Nothing
    }


{-| Creates a image transition between positions.
This is useful for images that will change their position during the game.

    image "enemy.png" |> movable "name"

**Note:**
The string should be unique, if not the transition might fail every now and then.

**Note:**
The string will be a id Attribute in a html node, so be careful not to use names that might be already taken.

-}
movable : String -> Image msg -> Image msg
movable transitionId contentElement =
    { contentElement
        | uniqueId = Just ( transitionId, True )
    }


{-| Pauses a the transition of a `movable` image.

**Only use in combination with `movable`:**

    image "teleportingEnemy.png" |> movable "name" |> jumping

Use this function if a tile has the `movable`-property, but you would like to
remove it without causing any unwanted side effects.

-}
jumping : Image msg -> Image msg
jumping ({ uniqueId } as t) =
    case uniqueId of
        Nothing ->
            t

        Just ( id, _ ) ->
            { t | uniqueId = Just ( id, False ) }


{-| `Tiles` are essentially also images,
therefore this constructor transforms a `Tile` and a `Tileset` into an `Image`.

    fromTile (tile ( 0, 0 ))
        (tileset
            { source = "tiles.png"
            , width = 80
            , height = 80
            }
        )
        == image "tiles.png"

**Note:**
`fromTile` displays only the `width` and `height` of the image, that where given.
This means setting `width` and `height` to `0` would not display the image at all.

    fromTile (tile ( 0, 0 ) |> movable "uniqueId")
        == fromTile (tile ( 0, 0 ))
        |> movable "uniqueId"

**Note:**
If you want to animate an `Image` use this function instead.

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


{-| Makes an `Image` clickable

Use this to create the `onClick` event from [Html.Events](https://package.elm-lang.org/packages/elm/html/latest/Html-Events#onClick).

-}
clickable : msg -> Image msg -> Image msg
clickable msg =
    withAttributes [ Events.onClick msg ]


{-| Adds a background color.

\*\* This makes the the Image non-transparent \*\*

This can be used to simulate monochrome sprites or to implement team colors.

-}
monochrome : Color -> Image msg -> Image msg
monochrome color =
    withAttributes
        [ color
            |> Color.toCssString
            |> Attributes.style "background-color"
        ]


{-| Adds custom attributes.

Use the [Html.Attributes](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes).

-}
withAttributes : List (Attribute msg) -> Image msg -> Image msg
withAttributes attributes ({ customAttributes } as i) =
    { i
        | customAttributes =
            attributes
                |> List.map Html.Styled.Attributes.fromUnstyled
                |> List.append customAttributes
    }


{-| [DEPRECATED]

Use `clickable` instead.

-}
onClick : msg -> Attribute msg
onClick msg =
    Events.onClick msg


{-| It is possible to compose an `Image` from a set of other images.
The two `Floats` are realtive coordinates.

    ((100,100),image "img.png")
    =
    ((20,50), multipleimages [((80,50),image "img.png")])

Sub-images loose the ability to be movable:

    multipleimages [((x,y),image "img.png" |> movable "id")]
    =
    multipleimages [((x,y),image "img.png")]

Instead use the following:

    image "img.png" |> movable "id"
    =
    multipleimages [((0,0),image "img.png")] |> movable "id"

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
