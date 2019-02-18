module PixelEngine.Graphics.Abstract exposing
    ( Compatible(..)
    , TilesetImage
    , render
    )

import Color
import Css exposing (px)
import Css.Global as Global
import Html.Styled as Html exposing (Attribute, Html, div)
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Keyed as Keyed
import PixelEngine.Graphics.Data exposing (Background(..), Dimensions)
import PixelEngine.Graphics.Data.Area exposing (Area(..), ImageAreaContent, TiledAreaContent)
import PixelEngine.Graphics.Data.Controller as Controller
import PixelEngine.Graphics.Data.Element exposing (Element(..), SingleSource(..))
import PixelEngine.Graphics.Data.Options exposing (Options(..))
import PixelEngine.Graphics.Data.Tile exposing (Tile)
import PixelEngine.Graphics.Data.Transition exposing (Transition(..))
import PixelEngine.Graphics.View.Element as ElementView


type alias TilesetImage msg =
    { source : String
    , tile : Tile msg
    }


type Compatible
    = Compatible


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
            (toFloat<|scale)
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
            (toFloat<|scale)
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


renderScreen : Options msg -> List (Area msg) -> Html msg
renderScreen options listOfArea =
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


render : Options msg -> List (Area msg) -> Html msg
render ((Options { width, scale, transitionFrom, transition, controllerOptions }) as options) to =
    let
        (Transition { name, transitionList }) =
            transition

        transitionLength : Float
        transitionLength =
            transitionList
                |> List.map Tuple.first
                |> List.sum

        animationCss =
            transitionList
                |> List.foldl
                    (\( length, cssCommands ) ( sum, string ) ->
                        ( sum + length
                        , string
                            ++ (String.fromFloat <| (sum + length) * 100 / transitionLength)
                            ++ "% {"
                            ++ "visibility:visible;"
                            ++ cssCommands
                            ++ "} "
                        )
                    )
                    ( 0, "" )
                |> Tuple.second
                |> (\a -> a ++ ";")
    in
    div
        [ css
            [ Css.backgroundColor (Css.rgb 0 0 0)
            ]
        ]
        ([ Global.global
            [ Global.selector
                ("@keyframes pixelengine_screen_transition_"
                    ++ name
                )
                [ Css.property
                    animationCss
                    ""
                ]
            ]
         , Html.node "meta"
            [ Attributes.name "viewport"
            , Attributes.attribute "content" "width=device-width, initial-scale=1, maximum-scale=1, user-scalable=no, user-scalable=0"
            ]
            []
         , Html.node "meta"
            [ Attributes.name "apple-mobile-web-app-capable"
            , Attributes.attribute "content" "yes"
            ]
            []
         , Html.node "meta"
            [ Attributes.name "apple-mobile-web-app-status-bar-style"
            , Attributes.attribute "content" "black-translucent"
            ]
            []
         , div
            [ css
                [ Css.position Css.relative
                , Css.width <| px <| (toFloat<|scale) * width
                , Css.margin Css.auto
                ]
            ]
            [ div
                []
                [ renderScreen options to ]
            , div
                [ css
                    [ Css.position Css.absolute
                    , Css.top <| px 0
                    , Css.left <| px 0
                    , Css.visibility Css.hidden
                    , Css.property "animation"
                        ("pixelengine_screen_transition_"
                            ++ name
                            ++ " "
                            ++ String.fromFloat transitionLength
                            ++ "s 1"
                        )
                    ]
                ]
                [ renderScreen options transitionFrom ]
            ]
         ]
            |> (case controllerOptions of
                    Nothing ->
                        identity

                    Just ({ windowSize } as justCtrlOptions) ->
                        if windowSize.width > 1.5 * windowSize.height then
                            \l -> Controller.render justCtrlOptions |> List.append l

                        else
                            identity
               )
        )
