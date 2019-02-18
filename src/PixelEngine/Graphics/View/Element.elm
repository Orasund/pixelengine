module PixelEngine.Graphics.View.Element exposing (displayMultiple, view)

import Css exposing (px)
import Html.Styled exposing (Attribute, Html, div, img)
import Html.Styled.Attributes exposing (css, src)
import PixelEngine.Graphics.Data exposing (Background(..), Location, Position)
import PixelEngine.Graphics.Data.Area exposing (Area(..), ContentElement)
import PixelEngine.Graphics.Data.Element
    exposing
        ( Element(..)
        , MultipleSources
        , SingleImage
        , SingleSource(..)
        , TileWithTileset
        )
import PixelEngine.Graphics.Data.Options exposing (Options(..))
import PixelEngine.Graphics.Data.Tile exposing (Tileset)


displayImage : Options msg -> ( Position, SingleImage ) -> Html msg
displayImage (Options { scale }) ( { top, left }, source ) =
    img
        [ src source
        , css
            [ Css.property "image-rendering" "pixelated"
            , Css.property "transform-origin" "top left"
            , Css.transform <| Css.scale2 (toFloat <| scale) (toFloat <| scale)
            , Css.position Css.absolute
            , Css.left (Css.px <| left)
            , Css.top (Css.px <| top)
            ]
        ]
        []



--------------------
-- display Tile
--------------------


displayStaticTile : Position -> Location -> Int -> Tileset -> Html msg
displayStaticTile pos ( spriteLeft, spriteTop ) scale { spriteWidth, spriteHeight, source } =
    img
        [ src source
        , css
            [ Css.property "object-fit" "none"
            , Css.property
                "object-position"
                (String.fromInt (-1 * spriteWidth * spriteLeft)
                    ++ "px "
                    ++ String.fromInt (-1 * spriteHeight * spriteTop)
                    ++ "px"
                )
            , Css.width <| px <| toFloat <| spriteWidth
            , Css.height <| px <| toFloat <| spriteHeight
            , Css.position Css.absolute
            , Css.top <| px <| pos.top
            , Css.left <| px <| pos.left
            , Css.property "image-rendering" "pixelated"
            , Css.property "transform-origin" "top left"
            , Css.transform <| Css.scale2 (toFloat <| scale) (toFloat <| scale)
            ]
        ]
        []


displayAnimatedTile : Position -> Location -> { scale : Int, steps : Int, fps : Float} -> Tileset -> Html msg
displayAnimatedTile pos ( spriteLeft, spriteTop ) { scale, steps,fps } { spriteWidth, spriteHeight, source } =
    div
        [ css
            [ Css.position Css.absolute
            , Css.top <| px <| pos.top
            , Css.left <| px <| pos.left
            , Css.width <| px <|  (toFloat <| scale *spriteWidth)
            , Css.height <| px <| (toFloat <| scale * spriteHeight)
            , Css.overflow Css.hidden
            ]
        ]
        [ div
            [ css
                [ Css.position Css.absolute
                , Css.right <|
                    px <|
                         (toFloat <| scale *spriteWidth * (steps + 2))
                ]
            ]
            [ img
                [ src source
                , css
                    [ Css.property "object-fit" "none"
                    , Css.property
                        "object-position"
                        (String.fromInt
                            (-1 * spriteWidth * spriteLeft)
                            ++ "px "
                            ++ String.fromInt (-1 * spriteHeight * spriteTop)
                            ++ "px"
                        )
                    , Css.width <|
                        px <|
                            toFloat <|
                                spriteWidth
                                    * (steps + 1)
                    , Css.height <| px <| toFloat <| spriteHeight
                    , Css.position Css.absolute
                    , Css.left <|
                        px <| (toFloat <| scale *spriteWidth * (steps + 1))
                    , Css.property "image-rendering" "pixelated"
                    , Css.property "animation"
                        ("pixelengine_graphics_basic "
                            ++ String.fromFloat ((toFloat (steps + 1) )/fps)
                            ++ ".0s steps("
                            ++ String.fromInt (steps + 1)
                            ++ ") infinite"
                        )
                    , Css.property "transform-origin" "top left"
                    , Css.transform <| Css.scale2 (toFloat<|scale) (toFloat<|scale)
                    , Css.float Css.right
                    ]
                ]
                []
            ]
        ]


displayTile : Options msg -> ( Position, TileWithTileset ) -> Html msg
displayTile (Options { scale,animationFPS }) ( pos, { left, top, steps, tileset } ) =
    if steps == 0 then
        displayStaticTile pos ( left, top ) scale tileset

    else
        displayAnimatedTile pos
            ( left, top )
            { scale = scale, steps = steps,fps = animationFPS }
            tileset



--------------------
-- View
--------------------


displayMultiple : Options msg -> ( Position, MultipleSources ) -> Maybe ( String, Bool ) -> List (Attribute msg) -> ( String, Html msg )
displayMultiple ((Options { scale, movementSpeedInSec }) as options) ( rootPosition, multipleSources ) transitionId attributes =
    ( transitionId |> Maybe.map Tuple.first |> Maybe.withDefault ""
    , div
        ((css <|
            List.concat
                [ case multipleSources of
                    [ ( _, TileSource { tileset } ) ] ->
                        let
                            { spriteWidth, spriteHeight } =
                                tileset
                        in
                        [ Css.width <|
                            Css.px <|
                                (toFloat <| scale * spriteWidth)
                        , Css.height <|
                            Css.px <|
                                 (toFloat <| scale *spriteHeight)
                        ]

                    _ ->
                        []
                , [ Css.position Css.absolute
                  , Css.left (Css.px <| (toFloat <| scale) * rootPosition.left)
                  , Css.top (Css.px <| (toFloat <| scale) * rootPosition.top)
                  ]
                , case transitionId of
                    Just ( _, True ) ->
                        [ Css.property "transition"
                            ("left "
                                ++ String.fromFloat movementSpeedInSec
                                ++ "s,top "
                                ++ String.fromFloat movementSpeedInSec
                                ++ "s;"
                            )
                        ]

                    _ ->
                        []
                ]
         )
            :: attributes
        )
        (multipleSources
            |> List.map
                (\( position, source ) ->
                    let
                        pos =
                            { top = (toFloat <| scale) * position.top
                            , left = (toFloat <| scale) * position.left
                            }
                    in
                    case source of
                        ImageSource imageSource ->
                            displayImage options ( pos, imageSource )

                        TileSource tileSource ->
                            displayTile options ( pos, tileSource )
                )
        )
    )


view : Options msg -> ( ( Float, Float ), ContentElement msg ) -> ( String, Html msg )
view options ( ( left, top ), { elementType, uniqueId, customAttributes } ) =
    let
        pos =
            { left = left, top = top }
    in
    case elementType of
        SingleSource singleSource ->
            displayMultiple
                options
                ( pos, [ ( { top = 0, left = 0 }, singleSource ) ] )
                uniqueId
                customAttributes

        MultipleSources multipleSources ->
            displayMultiple
                options
                ( pos, multipleSources )
                uniqueId
                customAttributes
