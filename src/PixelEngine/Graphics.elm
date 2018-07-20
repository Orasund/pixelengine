module PixelEngine.Graphics
    exposing
        ( Background(..)
        , Tile
        , Tileset
        , animatedMovableTile
        , animatedTile
        , movableTile
        , render
        , tile
        , tiledArea
        )

{-| Module description


## Functions

@docs animatedMovableTile,animatedTile,movableTile,render,tile,tiledArea


## Definition

@docs Background,Tile,Tileset

-}

import Css exposing (px)
import Css.Foreign as Foreign
import Html.Styled exposing (Html, div, img)
import Html.Styled.Attributes exposing (css, src)
import Html.Styled.Keyed as Keyed
import Pair


type alias Location =
    ( Int, Int )


type Tile
    = Tile
        { top : Int
        , left : Int
        , steps : Int
        , transitionId : Maybe String
        }


type alias Tileset =
    { source : String
    , width : Int
    , height : Int
    }


type Background
    = Color Css.Color
    | Image String


type alias TiledArea =
    { height : Int
    , tileset : Tileset
    , background : Background
    , content : List ( Location, Tile )
    }


type Area
    = Tiled TiledArea


type alias Config =
    { scale : Int
    , width : Int
    }


tiledArea : { height : Int, tileset : Tileset, background : Background } -> List ( Location, Tile ) -> Area
tiledArea { height, tileset, background } content =
    Tiled
        { height = height
        , tileset = tileset
        , background = background
        , content = content
        }


tile : Location -> Tile
tile ( left, top ) =
    Tile { top = top, left = left, steps = 0, transitionId = Nothing }


animatedTile : Location -> Int -> Tile
animatedTile ( left, top ) steps =
    Tile
        { top = top
        , left = left
        , steps =
            if steps > 0 then
                steps
            else
                0
        , transitionId = Nothing
        }


{-| Note: transitionId should be unique, if not the transition might fail every now and then.
-}
movableTile : Location -> String -> Tile
movableTile ( left, top ) id =
    Tile { top = top, left = left, steps = 0, transitionId = Just id }


{-| Note: transitionId should be unique, if not the transition might fail every now and then.
-}
animatedMovableTile : Location -> Int -> String -> Tile
animatedMovableTile ( left, top ) steps id =
    Tile
        { top = top
        , left = left
        , steps =
            if steps > 0 then
                steps
            else
                0
        , transitionId = Just id
        }


render : Config -> List Area -> Html msg
render config listOfArea =
    div [ css [ Css.backgroundColor (Css.rgb 0 0 0) ] ]
        (listOfArea
            |> List.foldl
                (\area list ->
                    case area of
                        Tiled tiledArea ->
                            [ renderTiledArea config tiledArea ]
                                |> List.append list
                )
                [ Foreign.global
                    [ Foreign.selector
                        "@keyframes pixelengine_graphics_basic"
                        [ Css.property "100% { right:0px };" "" ]
                    ]
                ]
        )


renderTiledArea : Config -> TiledArea -> Html msg
renderTiledArea { scale, width } { height, background, content, tileset } =
    div
        [ css
            (let
                style =
                    [ Css.width <| px <| toFloat <| scale * tileset.width * width
                    , Css.height <| px <| toFloat <| scale * tileset.height * height
                    , Css.margin Css.auto
                    , Css.position Css.relative
                    ]
             in
             case background of
                Color color ->
                    List.append style
                        [ Css.backgroundColor color ]

                Image image ->
                    List.append style
                        [ Css.backgroundImage (Css.url image)
                        , Css.backgroundRepeat Css.repeat
                        , Css.backgroundSize2 (px <| toFloat <| scale * tileset.width) (px <| toFloat <| scale * tileset.height)
                        , Css.property "image-rendering" "pixelated"
                        ]
            )
        ]
        (content
            |> List.partition (\( _, Tile { transitionId } ) -> transitionId == Nothing)
            |> Tuple.mapSecond (List.sortBy (\( _, Tile { transitionId } ) -> transitionId |> Maybe.withDefault ""))
            |> Pair.map (List.map (displayTile tileset scale))
            |> (\( noTransition, transition ) ->
                    [ div [] (noTransition |> List.map Tuple.second)
                    , Keyed.node "div" [] transition
                    ]
               )
        )


displayTile : Tileset -> Int -> ( Location, Tile ) -> ( String, Html msg )
displayTile { width, height, source } scale ( pos, Tile { left, top, steps, transitionId } ) =
    let
        ( x, y ) =
            pos

        ( i, j ) =
            ( left, top )
    in
    if steps == 0 then
        ( ""
        , img
            [ src source
            , css
                [ Css.property "object-fit" "none"
                , Css.property
                    "object-position"
                    (toString (-1 * width * i) ++ "px " ++ toString (-1 * height * j) ++ "px")
                , Css.width <| px <| toFloat <| width
                , Css.height <| px <| toFloat <| height
                , Css.position Css.absolute
                , Css.top <| px <| toFloat <| scale * height * y + height // 2
                , Css.left <| px <| toFloat <| scale * width * x + width // 2
                , Css.property "image-rendering" "pixelated"
                , Css.transform <| Css.scale2 scale scale
                ]
            ]
            []
        )
    else
        ( transitionId |> Maybe.withDefault ""
        , div
            [ css
                ((if transitionId == Nothing then
                    []
                  else
                    [ Css.property "transition" "left 0.2s,top 0.2s;"
                    ]
                 )
                    |> List.append
                        [ Css.position Css.absolute
                        , Css.top <| px <| toFloat <| scale * height * y
                        , Css.left <| px <| toFloat <| scale * width * x
                        , Css.width <| px <| toFloat <| scale * width
                        , Css.height <| px <| toFloat <| scale * height
                        , Css.overflow Css.hidden
                        ]
                )
            ]
            [ img
                [ src source
                , css
                    [ Css.property "object-fit" "none"
                    , Css.property
                        "object-position"
                        (toString (-1 * width * (i - 1)) ++ "px " ++ toString (-1 * height * j) ++ "px")
                    , Css.width <| px <| toFloat <| width * (steps + 2)
                    , Css.height <| px <| toFloat <| height
                    , Css.position Css.relative
                    , Css.right <| px <| toFloat <| width * 2 * (steps + 1)
                    , Css.marginLeft <| px <| toFloat <| width * (steps + 2) // 2
                    , Css.top <| px <| toFloat <| height // 2
                    , Css.property "image-rendering" "pixelated"
                    , Css.property "animation" ("pixelengine_graphics_basic " ++ toString (steps + 1) ++ ".0s steps(" ++ toString (steps + 1) ++ ") infinite")
                    , Css.transform <| Css.scale2 scale scale
                    ]
                ]
                []
            ]
        )
