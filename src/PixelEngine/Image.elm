module PixelEngine.Image exposing
    ( Image, fromSrc, fromTile, fromText, fromTextWithSpacing, map
    , movable, jumping, multipleImages, clickable, monochrome, withAttributes
    )

{-| This module contains functions for creating images.
These Images can then be used for the `imageArea` function from the <PixelEngine>


## Image

@docs Image, fromSrc, fromTile, fromText, fromTextWithSpacing, map


## Configurate

@docs movable, jumping, multipleImages, clickable, monochrome, withAttributes

-}

import Color exposing (Color)
import Html exposing (Attribute)
import Html.Attributes as Attributes
import Html.Events as Events
import Html.Styled.Attributes
import PixelEngine.Graphics.Data.Area as AreaData
import PixelEngine.Graphics.Data.Element as ElementData
import PixelEngine.Tile as Tile exposing (Tile, Tileset)


{-| A `Image` is actually a very general type: As we will see later,
even tiles are essentially images.
The following functions are intended to be modular.
-}
type alias Image msg =
    AreaData.ContentElement msg


{-| The basic image constructor.
The string contains the url to the image

    fromSrc "aStone.png"

-}
fromSrc : String -> Image msg
fromSrc source =
    { elementType =
        ElementData.SingleSource <|
            ElementData.ImageSource source
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

    fromTile (Tile.fromPosition ( 0, 0 ))
        (Tile.tileset
            { source = "tiles.png"
            , spriteWidth = 80
            , spriteHeight = 80
            }
        )
    = fromSrc "tiles.png"

**Note:**
`fromTile` displays only the `width` and `height` of the image, that where given.
This means setting `width` and `height` to `0` would not display the image at all.

    fromTile (Tile.fromPosition ( 0, 0 ) |> Tile.movable "uniqueId") tileset
        == fromTile (Tile.fromPosition ( 0, 0 )) tileset
        |> movable "uniqueId"

**Note:**
If you want to animate an `Image` use this function instead.

-}
fromTile : Tile msg -> Tileset -> Image msg
fromTile { info, uniqueId, customAttributes } tileset =
    info
        |> List.map
            (\{ top, left, steps } ->
                ( ( 0, 0 )
                , { elementType =
                        ElementData.SingleSource <|
                            ElementData.TileSource
                                { left = left
                                , top = top
                                , steps = steps
                                , tileset = tileset
                                }
                  , customAttributes = customAttributes
                  , uniqueId = uniqueId
                  }
                )
            )
        |> multipleImages


{-| Created an Image from a text-string and the Tileset of the font.

It only supports Ascii characters.

This package comes with a [collection of Fonts](https://github.com/Orasund/pixelengine/wiki/Collection-of-Fonts)
that are free to use.

-}
fromText : String -> Tileset -> Image msg
fromText text ({ spriteWidth } as tileset) =
    text
        |> Tile.fromText ( 0, 0 )
        |> List.indexedMap
            (\i tile ->
                ( ( toFloat <| i * spriteWidth, 0 ), fromTile tile tileset )
            )
        |> multipleImages


{-| Created an Image from a text-string and the Tileset of the font.

It only supports Ascii characters.

The first argument is the spaceing between letters. Use negative values to place
the letters nearer to echother.

This package comes with a [collection of Fonts](https://github.com/Orasund/pixelengine/wiki/Collection-of-Fonts)
that are free to use.

-}
fromTextWithSpacing : Float -> String -> Tileset -> Image msg
fromTextWithSpacing space text ({ spriteWidth } as tileset) =
    text
        |> Tile.fromText ( 0, 0 )
        |> List.indexedMap
            (\i tile ->
                ( ( toFloat i * (toFloat spriteWidth + space), 0 )
                , fromTile tile tileset
                )
            )
        |> multipleImages


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


{-| It is possible to compose an `Image` from a set of other images.
The two `Floats` are relative coordinates.

    ( ( 100, 100 ), fromSrc "img.png" )
        == ( ( 20, 50 ), multipleImages [ ( ( 80, 50 ), fromSrc "img.png" ) ] )

Sub-images loose the ability to be movable:

    multipleImages [((4,2),fromSrc "img.png" |> movable "id")]
    --> multipleImages [((4,2),fromSrc "img.png")]

Instead use the following:

    fromSrc "img.png"
        |> movable "id"
        == multipleImages [ ( ( 0, 0 ), fromSrc "img.png" ) ]
        |> movable "id"

-}
multipleImages : List ( ( Float, Float ), Image msg ) -> Image msg
multipleImages list =
    let
        images : ElementData.MultipleSources
        images =
            list
                |> List.foldr
                    (\( ( left, top ), contentElement ) ->
                        case contentElement.elementType of
                            ElementData.SingleSource singleSource ->
                                (::) ( { left = left, top = top }, singleSource )

                            ElementData.MultipleSources multipleSource ->
                                multipleSource
                                    |> List.map
                                        (\( pos, singleSource ) ->
                                            ( { left = left + pos.left
                                              , top = top + pos.top
                                              }
                                            , singleSource
                                            )
                                        )
                                    |> List.append
                    )
                    []
    in
    { elementType =
        ElementData.MultipleSources images
    , customAttributes = []
    , uniqueId = Nothing
    }


{-| Maps the message of an Image
-}
map : (a -> b) -> Image a -> Image b
map =
    AreaData.mapContentElement
