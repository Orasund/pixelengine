module PixelEngine.Graphics
    exposing
        ( Area
        , Background(..)
        , Config
        , Tile
        , Tileset
        , animatedMovableTile
        , animatedTile
        , movableTile
        , render
        , tile
        , tiledArea
        )

{-| A graphic engine for turn-based pixel games.

to get started, copy the following:

    import Css exposing (px)
    import Html.Styled exposing (toUnstyled)
    import PixelEngine.Graphics as Graphics exposing (Background(..), Tileset)

    main =
        let
            tileSize : Int
            tileSize =
                16

            windowSize : Int
            windowSize =
                16

            scale : Int
            scale =
                2

            width : Css.Px
            width =
                px <| toFloat <| (windowSize * tileSize * scale)

            tileset : Tileset
            tileset =
                { source = "tileset.png", width = 16, height = 16 }

            goblin =
                Graphics.animatedTile ( 2, 8 ) 1

            letter_h =
                Graphics.tile ( 1, 15 )

            letter_i =
                Graphics.tile ( 2, 12 )

            heart =
                Graphics.tile ( 4, 8 )
        in
        Graphics.render
            { scale = scale, width = width }
            [ Graphics.tiledArea { height = windowSize, tileset = tileset, background = Color (Css.rgb 20 12 28) }
                [ ( ( 6, 7 ), goblin )
                , ( ( 7, 7 ), letter_h )
                , ( ( 8, 7 ), letter_i )
                , ( ( 9, 7 ), heart )
                ]
            ]
            |> toUnstyled


## Definition

@docs Tileset,Background,Config


## Basic Functions

@docs render


## Tile

@docs Tile,animatedMovableTile,animatedTile,movableTile,tile


## Area

@docs Area,tiledArea

-}

import Css exposing (px)
import Css.Foreign as Foreign
import Html.Styled exposing (Html, div, img)
import Html.Styled.Attributes exposing (css, src)
import Html.Styled.Keyed as Keyed


type alias Location =
    ( Int, Int )


{-| a single tile of a tileset
-}
type Tile
    = Tile
        { top : Int
        , left : Int
        , steps : Int
        , transitionId : Maybe String
        }


{-| a tileset. It contains the link to the image as well as the size of a tile.

    {source: "tileset.png",width: 16, height 16}

-}
type alias Tileset =
    { source : String
    , width : Int
    , height : Int
    }


{-| possible backgrounds for an area.

  - Color - a single color, use the elm-css colors
    Color (Css.rgb 20 12 28)

  - Image - a image that gets tiled.
    Image "groundTile.png"

-}
type Background
    = Color Css.Color
    | Image String


type alias TiledArea =
    { height : Int
    , tileset : Tileset
    , background : Background
    , content : List ( Location, Tile )
    }


{-| an area of the window.
Elements in the area must be of the same type.
So for a tiled area contains only tiles of the same tileset
-}
type Area
    = Tiled TiledArea


{-| configurations of the engine.

  - scale - upscales all images (use scale = 1 for no scaleing)
  - width - width of the window. Use elm-css lengths.

```
{scale = 1,width = px 800}
```

-}
type alias Config compatible =
    { scale : Int
    , width : Css.LengthOrAuto compatible
    }


{-| creates a tiled area. Elements in this area are positioned on a grid.
The content consists of

  - (Int,Int) - position (x,y) in the area
  - Tile - a tile in the tileset

-}
tiledArea : { height : Int, tileset : Tileset, background : Background } -> List ( ( Int, Int ), Tile ) -> Area
tiledArea { height, tileset, background } content =
    Tiled
        { height = height
        , tileset = tileset
        , background = background
        , content = content
        }


{-| a basic tile
the first tile in a tileset is obtailed by
tile (0,0)
-}
tile : ( Int, Int ) -> Tile
tile ( left, top ) =
    Tile { top = top, left = left, steps = 0, transitionId = Nothing }


{-| an animated tile
the sprites of the animation must be arranged horizontally in the tileset

  - steps - steps of the animation (one less then the number of sprites.)

```
danimatedTile (0,0) 0 == tile (0,0)
```

-}
animatedTile : ( Int, Int ) -> Int -> Tile
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


{-| a movable tile
this means it will transition if the location gets chanced.

Note: the id should be unique, if not the transition might fail every now and then.

-}
movableTile : ( Int, Int ) -> String -> Tile
movableTile ( left, top ) id =
    Tile { top = top, left = left, steps = 0, transitionId = Just id }


{-| a animated movable tile
this means it will transition if the location gets chanced.

Note: the id should be unique, if not the transition might fail every now and then.

-}
animatedMovableTile : ( Int, Int ) -> Int -> String -> Tile
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


{-| renders the content. Use the elm-css Html in combination with this function.
-}
render : Config compatible -> List Area -> Html msg
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


renderTiledArea : Config compatible -> TiledArea -> Html msg
renderTiledArea { scale, width } { height, background, content, tileset } =
    div
        [ css
            (let
                style =
                    [ Css.width width
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
            |> pairMap (List.map (displayTile tileset scale))
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


pairMap : (a -> b) -> ( a, a ) -> ( b, b )
pairMap fun ( a, b ) =
    ( fun a, fun b )
