module PixelEngine.Graphics.Image exposing (Image, fromTile, image, movable, withAttributes)

{-| a

@docs Image,image,movable,fromTile,withAttributes

-}

import Html.Styled exposing (Attribute)
import PixelEngine.Graphics.Abstract as Abstract
import PixelEngine.Graphics.Tile exposing (Tile, Tileset)


{-| a content Element
-}
type alias Image msg =
    Abstract.ContentElement msg


{-| creates a image
-}
image : String -> Image msg
image source =
    { elementSource =
        Abstract.ImageSource source
    , customAttributes = []
    , uniqueId = Nothing
    }


{-| creates a movable image
-}
movable : String -> Image msg -> Image msg
movable transitionId contentElement =
    { contentElement
        | uniqueId = Just transitionId
    }


{-| imageFromTile
-}
fromTile : Tile msg -> Tileset -> Image msg
fromTile { info, uniqueId, customAttributes } tileset =
    let
        { top, left, steps } =
            info
    in
    { elementSource =
        Abstract.TileSource
            { left = left
            , top = top
            , steps = steps
            , tileset = tileset
            }
    , customAttributes = customAttributes
    , uniqueId = uniqueId
    }


{-| s
-}
withAttributes : List (Attribute msg) -> Image msg -> Image msg
withAttributes attributes image =
    { image
        | customAttributes = attributes
    }
