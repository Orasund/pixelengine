module PixelEngine exposing
    ( Area, tiledArea, imageArea, heightOf
    , PixelEngine
    , toHtml, game, gameWithNoControls, gameWithCustomControls
    , Background, imageBackground, colorBackground
    , Input(..), defaultInputs
    , basicControls, customControls, withMobileSupport
    )

{-| This module takes care of the Graphics.

You will want to add [PixelEngine.Graphics.Image](PixelEngine-Graphics-Image)
or [PixelEngine.Graphics.Tile](PixelEngine-Graphics-Tile) to
actually draw something.


# Area

The main idea of this graphic engine is to arrage the content into horizontal stripes,
so called areas.

@docs Area, tiledArea, imageArea, heightOf


# PixelEngine

@docs PixelEngine

If one wants to use just use this module on its own, you can use `toHtml` instead
of the `game` function from the main module.

@docs toHtml, game, gameWithNoControls, gameWithCustomControls


# Controls

The graphic engine provides a touch-controller for mobile devices.

@docs Input, defaultInputs

# Background

@docs Background, imageBackground, colorBackground


# Advanced

@docs basicControls, customControls, withMobileSupport
-}

import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import Color exposing (Color)
import Html exposing (Html)
import Html.Styled
import Json.Decode as Decode
import PixelEngine.Graphics.Abstract as Abstract
import PixelEngine.Graphics.Data as Data
import PixelEngine.Graphics.Data.Area as AreaData
import PixelEngine.Graphics.Data.Controller as ControllerData
import PixelEngine.Graphics.Data.Options as OptionsData
import PixelEngine.Image exposing (Image)
import PixelEngine.Options as Options exposing (Options)
import PixelEngine.Tile exposing (Tile, Tileset)
import Task


type alias Config msg =
    { windowSize :
        Maybe
            { width : Float
            , height : Float
            }
    , controls : Maybe ( String -> Maybe Input, Input -> Maybe msg )
    }


type alias Model model msg =
    { modelContent : model
    , config : Config msg
    }


type Msg msg
    = Resize { width : Float, height : Float }
    | MsgContent msg
    | DoNothing


batch : ( model, Cmd msg ) -> Model model msg -> ( Model model msg, Cmd (Msg msg) )
batch ( modelContent, msg ) { config } =
    ( { modelContent = modelContent, config = config }, msg |> Cmd.map MsgContent )


updateFunction : (msg -> model -> ( model, Cmd msg )) -> Msg msg -> Model model msg -> ( Model model msg, Cmd (Msg msg) )
updateFunction update msg ({ modelContent, config } as model) =
    case msg of
        Resize windowSize ->
            ( { model
                | config =
                    { config
                        | windowSize = Just windowSize
                    }
              }
            , Cmd.none
            )

        MsgContent msgC ->
            model |> batch (update msgC modelContent)

        DoNothing ->
            ( model
            , Cmd.none
            )


subscriptionsFunction : (model -> Sub msg) -> Model model msg -> Sub (Msg msg)
subscriptionsFunction subscriptions { modelContent, config } =
    Sub.batch
        ([ subscriptions modelContent |> Sub.map MsgContent
         , Events.onResize <| \w h -> Resize { width = toFloat w, height = toFloat h }
         ]
            |> List.append
                (case config.controls of
                    Just ( _, controlsToMsg ) ->
                        [ basicControls controlsToMsg
                            |> Sub.map
                                (\maybeMsg ->
                                    case maybeMsg of
                                        Just msg ->
                                            MsgContent msg

                                        Nothing ->
                                            DoNothing
                                )
                        ]

                    Nothing ->
                        []
                )
        )


viewFunction : (model -> { title : String, options : Maybe (Options msg), body : List (Area msg) }) -> Float -> Model model msg -> Browser.Document (Msg msg)
viewFunction view width { modelContent, config } =
    let
        { windowSize, controls } =
            config

        ({ title, body } as viewResult) =
            view modelContent

        ((OptionsData.Options { scale }) as options) =
            case viewResult.options of
                Just o ->
                    o

                Nothing ->
                    Options.default

        height =
            (toFloat <| scale) * heightOf body
    in
    { title = title
    , body =
        [ (case windowSize of
            Just wS ->
                toHtml
                    { width = width
                    , options =
                        Just
                            (options
                                |> OptionsData.usingScale
                                    (min (2 ^ (floor <| logBase 2 <| wS.height / height))
                                        (2 ^ (floor <| logBase 2 <| wS.width / width))
                                    )
                                |> (case controls of
                                        Just ( _, controlsToMsg ) ->
                                            withMobileSupport
                                                { windowSize = wS
                                                , controls = controlsToMsg
                                                }

                                        Nothing ->
                                            identity
                                   )
                            )
                    }
                    body

            Nothing ->
                toHtml { width = width, options = viewResult.options } []
          )
            |> Html.map MsgContent
        ]
    }


initFunction : Maybe ( String -> Maybe Input, Input -> Maybe msg ) -> (flags -> ( model, Cmd msg )) -> (flags -> ( Model model msg, Cmd (Msg msg) ))
initFunction controls init =
    \flag ->
        let
            ( modelContent, msg ) =
                init flag
        in
        ( { modelContent = modelContent
          , config = { windowSize = Nothing, controls = controls }
          }
        , Cmd.batch
            [ msg |> Cmd.map MsgContent
            , Task.perform
                (\{ viewport } ->
                    let
                        { width, height } =
                            viewport
                    in
                    Resize { width = width, height = height }
                )
                Dom.getViewport
            ]
        )


gameMaybeWithCustomControls :
    { init : flags -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , width : Float
    , view : model -> { title : String, options : Maybe (Options msg), body : List (Area msg) }
    , controls : Maybe ( String -> Maybe Input, Input -> Maybe msg )
    }
    -> Program flags (Model model msg) (Msg msg)
gameMaybeWithCustomControls { init, update, subscriptions, view, width, controls } =
    Browser.document
        { init =
            initFunction controls init
        , update =
            updateFunction update
        , subscriptions =
            subscriptionsFunction subscriptions
        , view =
            viewFunction view width
        }



--------------------------------------------------
-- exposing
--------------------------------------------------
----------------------
-- Area
----------------------


{-| A horizontal area of the content.
A `Area` defines how the content should be displayed.

**Note:**
An area can only contain elements of the same type.
So either you have tiles or images.

![A typical game](https://orasund.github.io/pixelengine/img4.png "A typical game")

-}
type alias Area msg =
    AreaData.Area msg


{-| Returns the height of a list of Areas

This can be used to return the height of a `tiledArea`.
For a `imageArea` this function is trivial.

-}
heightOf : List (Area msg) -> Float
heightOf listOfArea =
    List.sum
        (listOfArea
            |> List.map
                (\area ->
                    case area of
                        AreaData.Tiled { rows, tileset } ->
                            toFloat <| rows * tileset.spriteHeight

                        AreaData.Images { height } ->
                            height
                )
        )


{-| Every area has a background.
-}
type alias Background =
    Data.Background


{-| A single color background.
It uses [avh4/elm-color](https://package.elm-lang.org/packages/avh4/elm-color/latest).

```
colorBackground (Color.rgb255 20 12 28)
```

-}
colorBackground : Color -> Background
colorBackground color =
    Data.ColorBackground color


{-| An image that gets repeated.

```
Image "groundTile.png"
```

-}
imageBackground : { source : String, width : Float, height : Float } -> Background
imageBackground image =
    Data.ImageBackground image


{-| An area containing images that can be arranged freely.

This is a complete contrast to the way how `tiledArea` is working.
Usefull applications are GUIs, menus or loading screens.

Checkout [PixelEngine.Graphics.Image](PixelEngine-Graphics-Image) for more information.

This area has the following options:

  - `height` - The height or the `Area` in pixels.
  - `background` - The background of the `Area`.

-}
imageArea : { height : Float, background : Background } -> List ( ( Float, Float ), Image msg ) -> Area msg
imageArea { height, background } content =
    AreaData.Images
        { height = height
        , background = background
        , content = content
        }


{-| An area for using tilesets.

It supports one tileset at a time,
that means all sprites must be of the same size and stored as a grid in one single file.
This area is useful for displaying the playing field of a game.

Checkout [PixelEngine.Graphics.Tile](PixelEngine-Graphics-Image) for more information.

This area has the following options:

  - `rows` - The amount of rows of the grid. This value defines the height of the `Area`.
  - `tileset` - The tileset that will be used for all elements in the `Area`.
  - `background` - The background of the `Area`.

-}
tiledArea : { rows : Int, tileset : Tileset, background : Background } -> List ( ( Int, Int ), Tile msg ) -> Area msg
tiledArea { rows, tileset, background } content =
    AreaData.Tiled
        { rows = rows
        , tileset = tileset
        , background = background
        , content = content
        }


{-| Displays content of the game.
-}
toHtml : { width : Float, options : Maybe (Options msg) } -> List (Area msg) -> Html msg
toHtml { width, options } listOfArea =
    Abstract.render
        ((case options of
            Just o ->
                o

            Nothing ->
                Options.default
         )
            |> OptionsData.withWidth width
        )
        listOfArea
        |> Html.Styled.toUnstyled



----------------------
-- Controls
----------------------


{-| all possible Inputs.
-}
type Input
    = InputLeft
    | InputRight
    | InputUp
    | InputDown
    | InputA
    | InputB
    | InputX
    | InputY


{-| The default layout:

  - A/ArrowLeft - `InputLeft`
  - W/ArrowUp - `InputUp`
  - D/ArrowRight - `InputRight`
  - S/ArrowDown - `InputDown`
  - Space/Enter - `InputA`
  - X/Backspace/Esc - `InputB`
  - Q - `InputX`
  - E - `InputY`

-}
defaultInputs : String -> Maybe Input
defaultInputs =
    \string ->
        case string of
            "w" ->
                Just InputUp

            "W" ->
                Just InputUp

            "ArrowUp" ->
                Just InputUp

            "s" ->
                Just InputDown

            "S" ->
                Just InputDown

            "ArrowDown" ->
                Just InputDown

            "d" ->
                Just InputRight

            "D" ->
                Just InputRight

            "ArrowRight" ->
                Just InputRight

            "a" ->
                Just InputLeft

            "A" ->
                Just InputLeft

            "ArrowLeft" ->
                Just InputLeft

            " " ->
                Just InputA

            "Enter" ->
                Just InputA

            "q" ->
                Just InputX

            "Q" ->
                Just InputX

            "e" ->
                Just InputY

            "E" ->
                Just InputY

            "x" ->
                Just InputB

            "X" ->
                Just InputB

            "Escape" ->
                Just InputB

            "Backspace" ->
                Just InputB

            _ ->
                Nothing


{-| Subscribes to a keypress using custom key layouts.

It uses [key values](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) for the String.
You can use this [website](http://keycode.info) to find out the key value.

-}
customControls : (String -> Maybe Input) -> (Input -> Maybe msg) -> Sub (Maybe msg)
customControls decoder fun =
    Decode.field "key" Decode.string
        |> Decode.map decoder
        |> Decode.map (Maybe.andThen fun)
        |> Events.onKeyDown


{-| Subscribes to a keypress and sends the corresponding msg. This Function uses the default key layout.
-}
basicControls : (Input -> Maybe msg) -> Sub (Maybe msg)
basicControls fun =
    customControls defaultInputs fun



----------------------
-- PixelEngine
----------------------


{-| An alias for a PixelEngine program.

Your `main` function will have this type.

-}
type alias PixelEngine flag model msg =
    Program flag (Model model msg) (Msg msg)


{-| A game using custom controls.

The default controls should be enough to start,
but maybe you want to write a spelling game,
or its nessesary that very specifc keys are used?

-}
gameWithCustomControls :
    { init : flags -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , width : Float
    , view : model -> { title : String, options : Maybe (Options msg), body : List (Area msg) }
    , controls : ( String -> Maybe Input, Input -> Maybe msg )
    }
    -> Program flags (Model model msg) (Msg msg)
gameWithCustomControls { width, init, update, subscriptions, view, controls } =
    gameMaybeWithCustomControls
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , controls = Just controls
        , width = width
        }


{-| A game with no controls.

Use it just like `document` from [elm/browser](https://package.elm-lang.org/packages/elm/browser/latest/Browser).

-}
gameWithNoControls :
    { init : flags -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , width : Float
    , view : model -> { title : String, options : Maybe (Options msg), body : List (Area msg) }
    }
    -> Program flags (Model model msg) (Msg msg)
gameWithNoControls { init, width, update, subscriptions, view } =
    gameMaybeWithCustomControls
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , width = width
        , controls = Nothing
        }


{-| A prewired PixelEngine frame.

Use it just like `document` from [elm/browser](https://package.elm-lang.org/packages/elm/browser/latest/Browser).

-}
game :
    { init : flags -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : model -> { title : String, options : Maybe (Options msg), body : List (Area msg) }
    , controls : Input -> Maybe msg
    , width : Float
    }
    -> Program flags (Model model msg) (Msg msg)
game { init, update, width, subscriptions, view, controls } =
    gameMaybeWithCustomControls
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , width = width
        , controls = Just ( defaultInputs, controls )
        }


{-| Adds mobile support to the options.
It needs the window size.

<PixelEngine> provides a fully wired program that takes care of everything.

-}
withMobileSupport : { windowSize : { width : Float, height : Float }, controls : Input -> Maybe msg } -> Options msg -> Options msg
withMobileSupport { windowSize, controls } (OptionsData.Options options) =
    let
        { width, height } =
            windowSize

        convert : ControllerData.AbstractInput -> Input
        convert input =
            case input of
                ControllerData.AbstractInputA ->
                    InputA

                ControllerData.AbstractInputB ->
                    InputB

                ControllerData.AbstractInputX ->
                    InputX

                ControllerData.AbstractInputY ->
                    InputY

                ControllerData.AbstractInputUp ->
                    InputUp

                ControllerData.AbstractInputLeft ->
                    InputLeft

                ControllerData.AbstractInputRight ->
                    InputRight

                ControllerData.AbstractInputDown ->
                    InputDown
    in
    OptionsData.Options { options | controllerOptions = Just { windowSize = { width = width, height = height }, controls = convert >> controls } }
