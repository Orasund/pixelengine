module PixelEngine.Graphics.Abstract exposing (..)

import Css exposing (px)
import Css.Foreign as Foreign
import Html.Styled exposing (Attribute, Html, div, img)
import Html.Styled.Attributes exposing (css, src)
import Html.Styled.Keyed as Keyed


type alias TileInformation additional =
    { additional
        | top : Int
        , left : Int
        , steps : Int
    }


type alias Tile msg =
    { info : TileInformation {}
    , customAttributes : List (Attribute msg)
    , uniqueId : Maybe String
    }


type alias TilesetImage msg =
    { source : String
    , tile : Tile msg
    }


type alias Tileset =
    { source : String
    , spriteWidth : Int
    , spriteHeight : Int
    }


type Background
    = ColorBackground Css.Color
    | ImageBackground { source : String, width : Float, height : Float }


type alias Options options =
    { options
        | width : Float
        , scale : Float
        , transitionSpeedInSec : Float
    }


type alias ImageAreaContent msg =
    { height : Float
    , background : Background
    , content : List ( ( Float, Float ), ContentElement msg )
    }


type Area msg
    = Tiled (TiledAreaContent msg)
    | Images (ImageAreaContent msg)


type alias SingleImage =
    String


type ElementSource
    = TileSource TileWithTileset
    | ImageSource SingleImage


type alias ContentElement msg =
    { elementSource : ElementSource
    , customAttributes : List (Attribute msg)
    , uniqueId : Maybe String
    }


type alias TileWithTileset =
    TileInformation { tileset : Tileset }


type alias TiledAreaContent msg =
    { rows : Int
    , tileset : Tileset
    , background : Background
    , content : List ( Location, Tile msg )
    }


type alias Location =
    ( Int, Int )


type alias Position =
    { top : Float
    , left : Float
    }


type alias Dimensions =
    { width : Float
    , height : Float
    }


type Compatible
    = Compatible


renderFunction : Options {} -> List (Area msg) -> Html msg
renderFunction options listOfArea =
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
                [ Foreign.global
                    [ Foreign.selector
                        "@keyframes pixelengine_graphics_basic"
                        [ Css.property "100% { right:0px };" "" ]
                    ]
                ]
        )


renderTiledArea : Options {} -> TiledAreaContent msg -> Html msg
renderTiledArea ({ width, scale } as options) { rows, background, content, tileset } =
    let
        { spriteWidth, spriteHeight } =
            tileset
    in
    div
        [ cssArea
            scale
            background
            { width = width
            , height = scale * (toFloat <| tileset.spriteHeight * rows)
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
                        displayTile options
                            ( { left = scale * (toFloat <| spriteWidth * (location |> Tuple.first))
                              , top = scale * (toFloat <| spriteHeight * (location |> Tuple.second))
                              }
                            , { left = left
                              , top = top
                              , steps = steps
                              , tileset = tileset
                              }
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


renderImageArea : Options {} -> ImageAreaContent msg -> Html msg
renderImageArea ({ scale, width } as options) { height, background, content } =
    div
        [ cssArea
            scale
            background
            { width = width
            , height = height
            }
        ]
        (content
            |> List.partition (Tuple.second >> .uniqueId >> (==) Nothing)
            |> pairMap (List.map (displayElement options))
            |> (\( noTransition, transition ) ->
                    [ div [] (noTransition |> List.map Tuple.second)
                    , Keyed.node "div" [] (transition |> List.sortBy Tuple.first)
                    ]
               )
        )


cssArea : Float -> Background -> Dimensions -> Attribute msg
cssArea scale background { width, height } =
    css
        ((case background of
            ColorBackground color ->
                [ Css.backgroundColor color ]

            ImageBackground imageBackground ->
                cssBackgroundImage scale imageBackground.source { width = imageBackground.width, height = imageBackground.height }
         )
            |> List.append
                [ Css.width (Css.px <| width)
                , Css.height (Css.px <| height)
                ]
            |> List.append
                [ Css.margin Css.auto
                , Css.position Css.relative
                ]
        )


cssDimensions : Dimensions -> List Css.Style
cssDimensions { width, height } =
    [ Css.width (Css.px <| width)
    , Css.height (Css.px <| height)
    ]


cssBackgroundImage : Float -> String -> Dimensions -> List Css.Style
cssBackgroundImage scale image { width, height } =
    [ Css.backgroundImage (Css.url image)
    , Css.backgroundRepeat Css.repeat
    , Css.backgroundSize2 (Css.px <| width * scale) (Css.px <| height * scale)
    , Css.property "image-rendering" "pixelated"
    ]


cssPositions : Position -> List Css.Style
cssPositions { left, top } =
    [ Css.left (Css.px <| left)
    , Css.top (Css.px <| top)
    ]


displayElement : Options {} -> ( ( Float, Float ), ContentElement msg ) -> ( String, Html msg )
displayElement options ( ( left, top ), { elementSource, uniqueId, customAttributes } ) =
    let
        position =
            { left = left, top = top }
    in
    case elementSource of
        TileSource tileSource ->
            displayTile options ( position, tileSource ) uniqueId customAttributes

        ImageSource imageSource ->
            displayImage options ( position, imageSource ) uniqueId customAttributes


displayImage : Options {} -> ( Position, SingleImage ) -> Maybe String -> List (Attribute msg) -> ( String, Html msg )
displayImage { scale, transitionSpeedInSec } ( { top, left }, source ) transitionId attributes =
    ( transitionId |> Maybe.withDefault ""
    , img
        ([ src source
         , css
            ((if transitionId == Nothing then
                []
              else
                [ Css.property "transition" ("left " ++ toString transitionSpeedInSec ++ "s,top " ++ toString transitionSpeedInSec ++ "s;")
                ]
             )
                |> List.append
                    [ Css.position Css.absolute
                    , Css.property "image-rendering" "pixelated"
                    , Css.property "transform-origin" "top left"
                    , Css.transform <| Css.scale2 scale scale
                    ]
                |> List.append
                    [ Css.left (Css.px <| left)
                    , Css.top (Css.px <| top)
                    ]
            )
         ]
            |> List.append attributes
        )
        []
    )


displayTile : Options {} -> ( Position, TileWithTileset ) -> Maybe String -> List (Attribute msg) -> ( String, Html msg )
displayTile { scale, transitionSpeedInSec } ( pos, { left, top, steps, tileset } ) transitionId attributes =
    let
        ( i, j ) =
            ( left, top )

        { spriteWidth, spriteHeight, source } =
            tileset
    in
    if steps == 0 then
        ( transitionId |> Maybe.withDefault ""
        , img
            ([ src source
             , css
                ((if transitionId == Nothing then
                    []
                  else
                    [ Css.property "transition" ("left " ++ toString transitionSpeedInSec ++ "s,top " ++ toString transitionSpeedInSec ++ "s;")
                    ]
                 )
                    |> List.append
                        [ Css.property "object-fit" "none"
                        , Css.property
                            "object-position"
                            (toString (-1 * spriteWidth * i) ++ "px " ++ toString (-1 * spriteHeight * j) ++ "px")
                        , Css.width <| px <| toFloat <| spriteWidth
                        , Css.height <| px <| toFloat <| spriteHeight
                        , Css.position Css.absolute
                        , Css.top <| px <| pos.top --+ (toFloat <| spriteHeight // 2)
                        , Css.left <| px <| pos.left --+ (toFloat <| spriteWidth // 2)
                        , Css.property "image-rendering" "pixelated"
                        , Css.property "transform-origin" "top left"
                        , Css.transform <| Css.scale2 scale scale
                        ]
                )
             ]
                |> List.append attributes
            )
            []
        )
    else
        ( transitionId |> Maybe.withDefault ""
        , div
            ([ css
                ((if transitionId == Nothing then
                    []
                  else
                    [ Css.property "transition" ("left " ++ toString transitionSpeedInSec ++ "s,top " ++ toString transitionSpeedInSec ++ "s;")
                    ]
                 )
                    |> List.append
                        [ Css.position Css.absolute
                        , Css.top <| px <| pos.top
                        , Css.left <| px <| pos.left
                        , Css.width <| px <| scale * (toFloat <| spriteWidth)
                        , Css.height <| px <| scale * (toFloat <| spriteHeight)
                        , Css.overflow Css.hidden
                        ]
                )
             ]
                |> List.append attributes
            )
            [ img
                [ src source
                , css
                    [ Css.property "object-fit" "none"
                    , Css.property
                        "object-position"
                        (toString (-1 * spriteWidth * (i - 1)) ++ "px " ++ toString (-1 * spriteHeight * j) ++ "px")
                    , Css.width <| px <| toFloat <| spriteWidth * (steps + 2)
                    , Css.height <| px <| toFloat <| spriteHeight
                    , Css.position Css.relative
                    , Css.right <| px <| scale * (toFloat <| spriteWidth * (steps + 1))

                    --, Css.marginLeft <| px <| toFloat <| spriteWidth * (steps + 2)
                    --, Css.top <| px <| toFloat <| spriteHeight // 2
                    , Css.property "image-rendering" "pixelated"
                    , Css.property "animation" ("pixelengine_graphics_basic " ++ toString (steps + 1) ++ ".0s steps(" ++ toString (steps + 1) ++ ") infinite")
                    , Css.property "transform-origin" "top left"
                    , Css.transform <| Css.scale2 scale scale
                    ]
                ]
                []
            ]
        )


pairMap : (a -> b) -> ( a, a ) -> ( b, b )
pairMap fun ( a, b ) =
    ( fun a, fun b )
