module PixelEngine.Graphics
    exposing
        ( Area
        , AreaForUnit
        , Background(..)
        , Options
        , SupportedUnit(..)
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

To get started, copy the following project

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
                { source = "https://orasund.github.io/pixelengine/tileset.png", width = 16, height = 16 }

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
            [ Graphics.tiledArea
                { height = windowSize, tileset = tileset, background = Color (Css.rgb 20 12 28) }
                [ ( ( 6, 7 ), goblin )
                , ( ( 7, 7 ), letter_h )
                , ( ( 8, 7 ), letter_i )
                , ( ( 9, 7 ), heart )
                ]
            ]
            |> toUnstyled


## Definition

@docs Tileset,Background,Options


## Basic Functions

@docs render


## Tile

@docs Tile,animatedMovableTile,animatedTile,movableTile,tile


## Area

@docs Area,tiledArea


## Advanced

@docs SupportedUnit,AreaForUnit

-}

import Css exposing (px)
import Css.Foreign as Foreign
import Html.Styled exposing (Attribute, Html, div, img)
import Html.Styled.Attributes exposing (css, src)
import Html.Styled.Keyed as Keyed


type alias Location =
    ( Int, Int )


type alias Dimensions =
    { width : Float, height : Float, unit : SupportedUnit }


{-| Lengths can only be in one of the following units.
They are equivalent to the corresponding [elm-css units](http://package.elm-lang.org/packages/rtfeldman/elm-css/latest/Css#Length)
-}
type SupportedUnit
    = Px
    | Cm
    | Mm
    | In
    | Pc
    | Pt
    | Em
    | Rem


{-| A single tile of a tileset
-}
type Tile
    = Tile
        { top : Int
        , left : Int
        , steps : Int
        , transitionId : Maybe String
        }


{-| A tileset. It contains the link to the image as well as the size of a tile.

```
{source: "tileset.png",width: 16, height 16}
```

-}
type alias Tileset =
    { source : String
    , spriteWidth : Int
    , spriteHeight : Int
    }


{-| Possible backgrounds for an area.

  - Color - a single color, use the elm-css colors

```
Color (Css.rgb 20 12 28)
```

  - Image - a image that gets tiled.

```
Image "groundTile.png"
```

-}
type Background
    = Color Css.Color
    | Image { src : String, width : Float, height : Float }


type alias TiledAreaContent =
    { rows : Int
    , cols : Int
    , tileset : Tileset
    , background : Background
    , content : List ( Location, Tile )
    }


type alias ImageAreaContent =
    { height : ( Float, SupportedUnit )
    , background : Background
    , content : List ( ( Float, Float ), String )
    }


type AreaType
    = Tiled TiledAreaContent
    | Images ImageAreaContent


{-| An area of the window.
Elements in the area must be of the same type.
So for a tiled area contains only tiles of the same tileset.
-}
type alias Area compatible =
    { compatible
        | px : Compatible
        , area : AreaType
    }


{-| Areas supporting units other than pixels
-}
type alias AreaForUnit compatible =
    { compatible
        | unit : Compatible
        , area : AreaType
    }


type alias AreaForAny compatible =
    { compatible
        | area : AreaType
    }


type Compatible
    = Compatible


type alias TiledArea =
    AreaForAny
        { px : Compatible
        , tiledArea : Compatible
        }


type alias ImageArea =
    AreaForAny
        { unit : Compatible
        , tiledArea : Compatible
        }


{-| Content.

there are two options to pick from. The intended usecase is with pixels as unit.
If you want to use tilesets, this is the only way to use them.
But if not, then other units might be also interesting, thats why the second option exists.

-}
type Content compatible areaType
    = UsingPixels (List (Area compatible))
    | Using ( SupportedUnit, List (AreaForUnit compatible) )


{-| Options of the engine.

  - scale - Upscales all images (use scale = 1 for no scaleing).
  - width - Width of the window. Use [elm-css lengths](http://package.elm-lang.org/packages/rtfeldman/elm-css/latest/Css#Length).

```
{scale = 1,width = px 800}
```

-}
type alias Options options =
    { options
        | width : Float
    }


type alias FullOptions =
    { width : Float
    , unit : SupportedUnit
    }


{-| Creates a tiled area. Elements in this area are positioned on a grid.
The content consists of

  - (Int,Int) - position (x,y) in the area
  - Tile - a tile in the tileset

-}
tiledArea : { cols : Int, rows : Int, tileset : Tileset, background : Background } -> List ( ( Int, Int ), Tile ) -> TiledArea
tiledArea { cols, rows, tileset, background } content =
    { px = Compatible
    , tiledArea = Compatible
    , area =
        Tiled
            { rows = rows
            , cols = cols
            , tileset = tileset
            , background = background
            , content = content
            }
    }



{- imgArea : { height : ( Float, SupportedUnit ), background : Background } -> List ( ( Float, Float ), String ) -> Area
   imgArea { height, background } content =
       Images
           { height = height
           , background = background
           , content = content
           }
-}


{-| A basic tile.
The first tile in a tileset is obtailed by

```
tile (0,0)
```

-}
tile : ( Int, Int ) -> Tile
tile ( left, top ) =
    Tile { top = top, left = left, steps = 0, transitionId = Nothing }


{-| An animated tile.
The sprites of the animation must be arranged horizontally in the tileset.

  - steps - Steps of the animation (one less then the number of sprites.)

```
animatedTile (0,0) 0 == tile (0,0)
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


{-| A movable tile.
This means it will transition if the location gets chanced.

**Note:** the id should be a unique string, if not the transition might fail every now and then.

-}
movableTile : ( Int, Int ) -> String -> Tile
movableTile ( left, top ) id =
    Tile { top = top, left = left, steps = 0, transitionId = Just id }


{-| A animated movable tile.
This means it will transition if the location gets chanced.

**Note:** the id should be a unique string, if not the transition might fail every now and then.

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


{-| Renders the content.
Use the [elm-css Html](http://package.elm-lang.org/packages/rtfeldman/elm-css/latest/Html-Styled#Html) in combination with this function.
-}
render : Options {} -> List (Area compatible) -> Html msg
render { width } listOfArea =
    renderFunction { width = width, unit = Px } listOfArea


renderWithUnit : Options {} -> Content compatible areaType -> Html msg
renderWithUnit { width } content =
    case content of
        UsingPixels listOfArea ->
            renderFunction { width = width, unit = Px } listOfArea

        Using ( unit, listOfArea ) ->
            renderFunction { width = width, unit = unit } listOfArea


renderFunction : FullOptions -> List (AreaForAny compatible) -> Html msg
renderFunction ({ width } as options) listOfArea =
    div [ css [ Css.backgroundColor (Css.rgb 0 0 0) ] ]
        (listOfArea
            |> List.foldl
                (\({ area } as areaForAny) list ->
                    case area of
                        Tiled tiledAreaContent ->
                            [ renderTiledArea options tiledAreaContent ]
                                |> List.append list

                        _ ->
                            list
                 {- Images imgArea ->
                    [ renderImageArea config imgArea ]
                        |> List.append list
                 -}
                )
                [ Foreign.global
                    [ Foreign.selector
                        "@keyframes pixelengine_graphics_basic"
                        [ Css.property "100% { right:0px };" "" ]
                    ]
                ]
        )



{- renderImageArea : Config -> ImageArea -> Html msg
   renderImageArea { scale, width, unit } { height, background, content } =
       div
           [ cssArea scale
               background
               { width = width, height = height, unit = unit }
           ]
           content
           |> List.map displayImage
-}


renderTiledArea : Options options -> TiledAreaContent -> Html msg
renderTiledArea { width } { rows, cols, background, content, tileset } =
    let
        scale : Float
        scale =
            width / (toFloat <| tileset.spriteWidth * cols)
    in
    div
        [ cssArea
            background
            { width = width
            , height = scale * (toFloat <| tileset.spriteHeight * rows)
            , unit = Px
            }
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


cssArea : Background -> Dimensions -> Attribute msg
cssArea background dimensions =
    css
        ((case background of
            Color color ->
                [ Css.backgroundColor color ]

            Image { src, width, height } ->
                cssBackgroundImage
                    src
                    { width = width
                    , height = height
                    , unit = Px
                    }
         )
            |> List.append (cssDimensions dimensions)
            |> List.append
                [ Css.margin Css.auto
                , Css.position Css.relative
                ]
        )


cssDimensions : Dimensions -> List Css.Style
cssDimensions { width, height, unit } =
    case unit of
        Px ->
            [ Css.width (Css.px <| width)
            , Css.height (Css.px <| height)
            ]

        Cm ->
            [ Css.width (Css.cm <| width)
            , Css.height (Css.cm <| height)
            ]

        Mm ->
            [ Css.width (Css.mm <| width)
            , Css.height (Css.mm <| height)
            ]

        In ->
            [ Css.width (Css.inches <| width)
            , Css.height (Css.inches <| height)
            ]

        Pc ->
            [ Css.width (Css.pc <| width)
            , Css.height (Css.pc <| height)
            ]

        Pt ->
            [ Css.width (Css.pt <| width)
            , Css.height (Css.pt <| height)
            ]

        Em ->
            [ Css.width (Css.em <| width)
            , Css.height (Css.em <| height)
            ]

        Rem ->
            [ Css.width (Css.rem <| width)
            , Css.height (Css.rem <| height)
            ]


cssPositions : { top : Float, left : Float, unit : SupportedUnit } -> List Css.Style
cssPositions { left, top, unit } =
    case unit of
        Px ->
            [ Css.left (Css.px <| left)
            , Css.top (Css.px <| top)
            ]

        Cm ->
            [ Css.left (Css.cm <| left)
            , Css.top (Css.cm <| top)
            ]

        Mm ->
            [ Css.left (Css.mm <| left)
            , Css.top (Css.mm <| top)
            ]

        In ->
            [ Css.left (Css.inches <| left)
            , Css.top (Css.inches <| top)
            ]

        Pc ->
            [ Css.left (Css.pc <| left)
            , Css.top (Css.pc <| top)
            ]

        Pt ->
            [ Css.left (Css.pt <| left)
            , Css.top (Css.pt <| top)
            ]

        Em ->
            [ Css.left (Css.em <| left)
            , Css.top (Css.em <| top)
            ]

        Rem ->
            [ Css.left (Css.rem <| left)
            , Css.top (Css.rem <| top)
            ]


cssPositionsAnimated : { top : Float, marginLeft : Float, right : Float, unit : SupportedUnit } -> List Css.Style
cssPositionsAnimated { right, top, marginLeft, unit } =
    case unit of
        Px ->
            [ Css.right (Css.px <| right)
            , Css.top (Css.px <| top)
            , Css.marginLeft (Css.px <| marginLeft)
            ]

        Cm ->
            [ Css.right (Css.cm <| right)
            , Css.top (Css.cm <| top)
            , Css.marginLeft (Css.cm <| marginLeft)
            ]

        Mm ->
            [ Css.right (Css.mm <| right)
            , Css.top (Css.mm <| top)
            , Css.marginLeft (Css.mm <| marginLeft)
            ]

        In ->
            [ Css.right (Css.inches <| right)
            , Css.top (Css.inches <| top)
            , Css.marginLeft (Css.inches <| marginLeft)
            ]

        Pc ->
            [ Css.right (Css.pc <| right)
            , Css.top (Css.pc <| top)
            , Css.marginLeft (Css.pc <| marginLeft)
            ]

        Pt ->
            [ Css.right (Css.pt <| right)
            , Css.top (Css.pt <| top)
            , Css.marginLeft (Css.pt <| marginLeft)
            ]

        Em ->
            [ Css.right (Css.em <| right)
            , Css.top (Css.em <| top)
            , Css.marginLeft (Css.em <| marginLeft)
            ]

        Rem ->
            [ Css.right (Css.rem <| right)
            , Css.top (Css.rem <| top)
            , Css.marginLeft (Css.rem <| marginLeft)
            ]


cssBackgroundImage : String -> Dimensions -> List Css.Style
cssBackgroundImage image { width, height, unit } =
    let
        backgroundSize : SupportedUnit -> Float -> Float -> Css.Style
        backgroundSize u w h =
            case u of
                Px ->
                    Css.backgroundSize2 (Css.px <| w) (Css.px <| h)

                Cm ->
                    Css.backgroundSize2 (Css.cm <| w) (Css.cm <| h)

                Mm ->
                    Css.backgroundSize2 (Css.mm <| w) (Css.mm <| h)

                In ->
                    Css.backgroundSize2 (Css.inches <| w) (Css.inches <| h)

                Pc ->
                    Css.backgroundSize2 (Css.pc <| w) (Css.pc <| h)

                Pt ->
                    Css.backgroundSize2 (Css.pt <| w) (Css.pt <| h)

                Em ->
                    Css.backgroundSize2 (Css.em <| w) (Css.em <| h)

                Rem ->
                    Css.backgroundSize2 (Css.rem <| w) (Css.rem <| h)
    in
    [ Css.backgroundImage (Css.url image)
    , Css.backgroundRepeat Css.repeat
    , backgroundSize unit width height
    , Css.property "image-rendering" "pixelated"
    ]



{- displayImage :  Float -> SupportedUnit -> (Location,{ src : String, width : Float, height : Float }) -> Html msg
   displayImage scale unit (pos,{src,width,height}) =
-}


displayTile : Tileset -> Float -> ( Location, Tile ) -> ( String, Html msg )
displayTile { spriteWidth, spriteHeight, source } scale ( pos, Tile { left, top, steps, transitionId } ) =
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
                    (toString (-1 * spriteWidth * i) ++ "px " ++ toString (-1 * spriteHeight * j) ++ "px")
                , Css.width <| px <| toFloat <| spriteWidth
                , Css.height <| px <| toFloat <| spriteHeight
                , Css.position Css.absolute
                , Css.top <| px <| scale * (toFloat <| spriteHeight * y) + (toFloat <| spriteHeight // 2)
                , Css.left <| px <| scale * (toFloat <| spriteWidth * x) + (toFloat <| spriteWidth // 2)
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
                        , Css.top <| px <| scale * (toFloat <| spriteHeight * y)
                        , Css.left <| px <| scale * (toFloat <| spriteWidth * x)
                        , Css.width <| px <| scale * (toFloat <| spriteWidth)
                        , Css.height <| px <| scale * (toFloat <| spriteHeight)
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
                        (toString (-1 * spriteWidth * (i - 1)) ++ "px " ++ toString (-1 * spriteHeight * j) ++ "px")
                    , Css.width <| px <| toFloat <| spriteWidth * (steps + 2)
                    , Css.height <| px <| toFloat <| spriteHeight
                    , Css.position Css.relative
                    , Css.right <| px <| toFloat <| spriteWidth * 2 * (steps + 1)
                    , Css.marginLeft <| px <| toFloat <| spriteWidth * (steps + 2) // 2
                    , Css.top <| px <| toFloat <| spriteHeight // 2
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
