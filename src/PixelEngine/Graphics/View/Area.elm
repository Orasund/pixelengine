module PixelEngine.Graphics.View.Area exposing (view)

import Color
import Css exposing (px)
import Css.Global as Global
import Html.Styled exposing (Attribute, Html, div)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Keyed as Keyed
import PixelEngine.Graphics.Data exposing (Background(..), Dimensions)
import PixelEngine.Graphics.Data.Area exposing (Area(..), ImageAreaContent, TiledAreaContent)
import PixelEngine.Graphics.Data.Element exposing (Element(..), SingleSource(..))
import PixelEngine.Graphics.Data.Options exposing (Options(..))
import PixelEngine.Graphics.Data.Transition exposing (Transition(..))
import PixelEngine.Graphics.View.Element as ElementView


pairMap : (a -> b) -> ( a, a ) -> ( b, b )
pairMap fun ( a, b ) =
    ( fun a, fun b )


cssBackgroundImage : Float -> String -> Dimensions -> List Css.Style
cssBackgroundImage scale image { width, height } =
    [ Css.backgroundImage (Css.url image)
    , Css.backgroundRepeat Css.repeat
    , Css.backgroundSize2 (Css.px <| width * scale) (Css.px <| height * scale)
    , Css.property "image-rendering" "pixelated"
    ]



--------------------
-- Area
--------------------


cssArea : Float -> Background -> Dimensions -> Attribute msg
cssArea scale background { width, height } =
    css
        ((case background of
            ColorBackground color ->
                [ color
                    |> Color.toCssString
                    |> Css.property "background-color"
                ]

            ImageBackground imageBackground ->
                cssBackgroundImage scale imageBackground.source { width = imageBackground.width, height = imageBackground.height }
         )
            |> List.append
                [ Css.width (Css.px <| scale * width)
                , Css.height (Css.px <| scale * height)
                ]
            |> List.append
                [ Css.margin Css.auto
                , Css.position Css.relative
                ]
        )


renderTiledArea : Options msg -> TiledAreaContent msg -> Html msg
renderTiledArea ((Options { width, scale }) as options) { rows, background, content, tileset } =
    let
        { spriteWidth, spriteHeight } =
            tileset
    in
    div
        [ cssArea
            (toFloat <| scale)
            background
            { width = width
            , height = toFloat <| tileset.spriteHeight * rows
            }
        ]
        (content
            |> List.partition (Tuple.second >> .uniqueId >> (==) Nothing)
            |> pairMap
                (List.map
                    (\( location, { info, uniqueId, customAttributes } ) ->
                        let
                            { top, left, steps } =
                                info
                        in
                        ElementView.displayMultiple options
                            ( { left = toFloat <| spriteWidth * (location |> Tuple.first)
                              , top = toFloat <| spriteHeight * (location |> Tuple.second)
                              }
                            , [ ( { top = 0, left = 0 }
                                , TileSource
                                    { left = left
                                    , top = top
                                    , steps = steps
                                    , tileset = tileset
                                    }
                                )
                              ]
                            )
                            uniqueId
                            customAttributes
                    )
                )
            |> (\( noTransition, transition ) ->
                    [ div [] (noTransition |> List.map Tuple.second)
                    , Keyed.node "div" [] (transition |> List.sortBy Tuple.first)
                    ]
               )
        )


renderImageArea : Options msg -> ImageAreaContent msg -> Html msg
renderImageArea ((Options { scale, width }) as options) { height, background, content } =
    div
        [ cssArea
            (toFloat <| scale)
            background
            { width = width
            , height = height
            }
        ]
        (content
            |> List.partition (Tuple.second >> .uniqueId >> (==) Nothing)
            |> pairMap (List.map (ElementView.view options))
            |> (\( noTransition, transition ) ->
                    [ div [] (noTransition |> List.map Tuple.second)
                    , Keyed.node "div" [] (transition |> List.sortBy Tuple.first)
                    ]
               )
        )


view : Options msg -> List (Area msg) -> Html msg
view options listOfArea =
    div [ css [ Css.backgroundColor (Css.rgb 0 0 0) ] ]
        (listOfArea
            |> List.foldl
                (\area list ->
                    case area of
                        Tiled tiledAreaContent ->
                            [ renderTiledArea options tiledAreaContent ]
                                |> List.append list

                        Images imageAreaContent ->
                            [ renderImageArea options imageAreaContent ]
                                |> List.append list
                )
                [ Global.global
                    [ Global.selector
                        "@keyframes pixelengine_graphics_basic"
                        [ Css.property "100% { left:0px };" "" ]
                    ]
                ]
        )
