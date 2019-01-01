module PixelEngine.Graphics.Abstract exposing (AbstractInput(..), Area(..), Background(..), Compatible(..), ContentElement, ControllerOptions, Dimensions, ElementType(..), ImageAreaContent, Location, MultipleSources, Options(..), Position, SingleImage, SingleSource(..), Tile, TileInformation, TileWithTileset, TiledAreaContent, Tileset, TilesetImage, Transition(..), cssArea, cssBackgroundImage, cssDimensions, cssPositions, displayElement, displayImage, displayMultiple, displayTile, newOptions, pairMap, render, renderControls, renderImageArea, renderScreen, renderTiledArea)

import Css exposing (px)
import Css.Global as Global
import Html.Styled as Html exposing (Attribute, Html, div, img)
import Html.Styled.Attributes as Attributes exposing (css, src)
import Html.Styled.Events as Events
import Html.Styled.Keyed as Keyed


type alias TileInformation additional =
    { additional
        | top : Int
        , left : Int
        , steps : Int
    }


type AbstractInput
    = AbstractInputLeft
    | AbstractInputRight
    | AbstractInputUp
    | AbstractInputDown
    | AbstractInputA
    | AbstractInputB
    | AbstractInputX
    | AbstractInputY
    | AbstractInputNone


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


type Transition
    = Transition { name : String, transitionList : List ( Float, String ) }


type Background
    = ColorBackground Css.Color
    | ImageBackground { source : String, width : Float, height : Float }


type alias ControllerOptions msg =
    { windowSize : Dimensions
    , controls : AbstractInput -> msg
    }


type Options msg
    = Options
        { width : Float
        , scale : Float
        , transitionSpeedInSec : Float
        , controllerOptions : Maybe (ControllerOptions msg)
        , transitionFrom : List (Area msg)
        , transition : Transition
        }


newOptions : { width : Float, scale : Float, transitionSpeedInSec : Float } -> Options msg
newOptions { width, scale, transitionSpeedInSec } =
    Options
        { width = width
        , scale = scale
        , transitionSpeedInSec = transitionSpeedInSec
        , controllerOptions = Nothing
        , transitionFrom = []
        , transition = Transition { name = "", transitionList = [ ( 0, "" ) ] }
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


type alias MultipleSources =
    List ( Position, SingleSource )


type SingleSource
    = TileSource TileWithTileset
    | ImageSource SingleImage


type ElementType
    = SingleSource SingleSource
    | MultipleSources MultipleSources


type alias ContentElement msg =
    { elementType : ElementType
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
                , Css.width <| px <| scale * width
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
                            \l -> renderControls justCtrlOptions |> List.append l

                        else
                            identity
               )
        )


renderControls : ControllerOptions msg -> List (Html msg)
renderControls { windowSize, controls } =
    let
        diameter : Float
        diameter =
            windowSize.height / 4

        circle : String -> AbstractInput -> Float -> List Css.Style -> Html msg
        circle char input size listOfCss =
            div
                [ css
                    ([ Css.width <| Css.px <| size
                     , Css.height <| Css.px <| size
                     , Css.borderRadius <| Css.px <| size / 2
                     , Css.backgroundColor <| Css.rgb 256 256 256
                     , Css.textAlign Css.center
                     , Css.fontFamily Css.sansSerif
                     , Css.fontSize <| Css.px <| size * 0.9
                     , Css.opacity <| Css.num 0.5
                     ]
                        |> List.append listOfCss
                    )
                , Events.onClick <| controls <| input
                ]
                [ Html.text <| char ]
    in
    [ div
        [ css
            [ Css.position Css.absolute
            , Css.top <| px 0
            , Css.left <| px 0
            ]
        ]
        [ circle "◀"
            AbstractInputLeft
            diameter
            [ Css.position Css.absolute
            , Css.top <| px <| diameter * 1.5
            , Css.left <| px 0
            ]
        , circle "▶"
            AbstractInputRight
            diameter
            [ Css.position Css.absolute
            , Css.top <| px <| diameter * 1.5
            , Css.left <| px <| diameter + (sqrt (2 * diameter ^ 2) - diameter)
            ]
        , circle "▲"
            AbstractInputUp
            diameter
            [ Css.position Css.absolute
            , Css.top <| px <| diameter * 1.0 - (sqrt (2 * diameter ^ 2) - diameter) / 2
            , Css.left <| px <| diameter * 0.5 + (sqrt (2 * diameter ^ 2) - diameter) / 2
            ]
        , circle "▼"
            AbstractInputDown
            diameter
            [ Css.position Css.absolute
            , Css.top <| px <| diameter * 2.0 + (sqrt (2 * diameter ^ 2) - diameter) / 2
            , Css.left <| px <| diameter * 0.5 + (sqrt (2 * diameter ^ 2) - diameter) / 2
            ]
        ]
    , div
        [ css
            [ Css.position Css.absolute
            , Css.top <| px 0
            , Css.right <| px 0
            ]
        ]
        [ circle "A"
            AbstractInputA
            diameter
            [ Css.position Css.absolute
            , Css.top <| px <| diameter * 1.5
            , Css.right <| px 0
            ]
        , circle "X"
            AbstractInputX
            diameter
            [ Css.position Css.absolute
            , Css.top <| px <| diameter * 1.5
            , Css.right <| px <| diameter + (sqrt (2 * diameter ^ 2) - diameter)
            ]
        , circle "Y"
            AbstractInputY
            diameter
            [ Css.position Css.absolute
            , Css.top <| px <| diameter * 1.0 - (sqrt (2 * diameter ^ 2) - diameter) / 2
            , Css.right <| px <| diameter * 0.5 + (sqrt (2 * diameter ^ 2) - diameter) / 2
            ]
        , circle "B"
            AbstractInputB
            diameter
            [ Css.position Css.absolute
            , Css.top <| px <| diameter * 2.0 + (sqrt (2 * diameter ^ 2) - diameter) / 2
            , Css.right <| px <| diameter * 0.5 + (sqrt (2 * diameter ^ 2) - diameter) / 2
            ]
        ]
    ]


renderTiledArea : Options msg -> TiledAreaContent msg -> Html msg
renderTiledArea ((Options { width, scale }) as options) { rows, background, content, tileset } =
    let
        { spriteWidth, spriteHeight } =
            tileset
    in
    div
        [ cssArea
            scale
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
                        displayMultiple options
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
                [ Css.width (Css.px <| scale * width)
                , Css.height (Css.px <| scale * height)
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


displayElement : Options msg -> ( ( Float, Float ), ContentElement msg ) -> ( String, Html msg )
displayElement ((Options { scale }) as options) ( ( left, top ), { elementType, uniqueId, customAttributes } ) =
    let
        pos =
            { left = left, top = top }
    in
    case elementType of
        {- SingleSource (TileSource tileSource) ->
               displayTile options ( position, tileSource ) uniqueId customAttributes

           SingleSource (ImageSource imageSource) ->
               displayImage options ( position, imageSource ) uniqueId customAttributes
        -}
        SingleSource singleSource ->
            displayMultiple options ( pos, [ ( { top = 0, left = 0 }, singleSource ) ] ) uniqueId customAttributes

        MultipleSources multipleSources ->
            displayMultiple options ( pos, multipleSources ) uniqueId customAttributes


displayMultiple : Options msg -> ( Position, MultipleSources ) -> Maybe String -> List (Attribute msg) -> ( String, Html msg )
displayMultiple ((Options { scale, transitionSpeedInSec }) as options) ( rootPosition, multipleSources ) transitionId attributes =
    ( transitionId |> Maybe.withDefault ""
    , div
        ([ css
            ((if transitionId == Nothing then
                []

              else
                [ Css.property "transition" ("left " ++ String.fromFloat transitionSpeedInSec ++ "s,top " ++ String.fromFloat transitionSpeedInSec ++ "s;")
                ]
             )
                |> List.append
                    [ Css.position Css.absolute
                    , Css.left (Css.px <| scale * rootPosition.left)
                    , Css.top (Css.px <| scale * rootPosition.top)
                    ]
                |> (case multipleSources of
                        [ ( _, TileSource { tileset } ) ] ->
                            let
                                { spriteWidth, spriteHeight } =
                                    tileset
                            in
                            List.append
                                [ Css.width <| Css.px <| scale * (toFloat <| spriteWidth)
                                , Css.height <| Css.px <| scale * (toFloat <| spriteHeight)
                                ]

                        _ ->
                            identity
                   )
            )
         ]
            |> List.append attributes
        )
        (multipleSources
            |> List.map
                (\( position, source ) ->
                    let
                        pos =
                            { top = scale * position.top
                            , left = scale * position.left
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


displayImage : Options msg -> ( Position, SingleImage ) -> Html msg
displayImage (Options { scale, transitionSpeedInSec }) ( { top, left }, source ) =
    img
        [ src source
        , css
            [ Css.property "image-rendering" "pixelated"
            , Css.property "transform-origin" "top left"
            , Css.transform <| Css.scale2 scale scale
            , Css.position Css.absolute
            , Css.left (Css.px <| left)
            , Css.top (Css.px <| top)
            ]
        ]
        []


displayTile : Options msg -> ( Position, TileWithTileset ) -> Html msg
displayTile (Options { scale, transitionSpeedInSec }) ( pos, { left, top, steps, tileset } ) =
    let
        ( i, j ) =
            ( left, top )

        { spriteWidth, spriteHeight, source } =
            tileset
    in
    if steps == 0 then
        img
            [ src source
            , css
                [ Css.property "object-fit" "none"
                , Css.property
                    "object-position"
                    (String.fromInt (-1 * spriteWidth * i) ++ "px " ++ String.fromInt (-1 * spriteHeight * j) ++ "px")
                , Css.width <| px <| toFloat <| spriteWidth
                , Css.height <| px <| toFloat <| spriteHeight
                , Css.position Css.absolute
                , Css.top <| px <| pos.top --+ (toFloat <| spriteHeight // 2)
                , Css.left <| px <| pos.left --+ (toFloat <| spriteWidth // 2)
                , Css.property "image-rendering" "pixelated"
                , Css.property "transform-origin" "top left"
                , Css.transform <| Css.scale2 scale scale
                ]
            ]
            []

    else
        div
            [ css
                [ Css.position Css.absolute
                , Css.top <| px <| pos.top
                , Css.left <| px <| pos.left
                , Css.width <| px <| scale * (toFloat <| spriteWidth)
                , Css.height <| px <| scale * (toFloat <| spriteHeight)
                , Css.overflow Css.hidden
                ]
            ]
            [ div
                [ css
                    [ Css.position Css.absolute
                    , Css.right <| px <| scale * (toFloat <| spriteWidth * (steps + 2))
                    ]
                ]
                [ img
                    [ src source
                    , css
                        [ Css.property "object-fit" "none"
                        , Css.property
                            "object-position"
                            (String.fromInt(-1 * spriteWidth * i) ++ "px " ++ String.fromInt (-1 * spriteHeight * j) ++ "px")
                        , Css.width <| px <| toFloat <| spriteWidth * (steps + 1)
                        , Css.height <| px <| toFloat <| spriteHeight
                        , Css.position Css.absolute
                        , Css.left <| px <| scale * (toFloat <| spriteWidth * (steps + 1))
                        , Css.property "image-rendering" "pixelated"
                        , Css.property "animation" ("pixelengine_graphics_basic " ++ String.fromInt (steps + 1) ++ ".0s steps(" ++ String.fromInt (steps + 1) ++ ") infinite")
                        , Css.property "transform-origin" "top left"
                        , Css.transform <| Css.scale2 scale scale
                        , Css.float Css.right
                        ]
                    ]
                    []
                ]
            ]



pairMap : (a -> b) -> ( a, a ) -> ( b, b )
pairMap fun ( a, b ) =
    ( fun a, fun b )
