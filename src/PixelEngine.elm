module PixelEngine exposing (PixelEngine, program, programWithCustomControls)

{-| The Libary may support more then just simple rendering. This module exists to take care of everything else.
It provides a program that is already set up.

@docs PixelEngine, program, programWithCustomControls

-}

import Browser
import Browser.Events as Events
import Browser.Dom as Dom
import Html exposing (Html)
import PixelEngine.Controls as Controls exposing (Input)
import PixelEngine.Graphics as Graphics exposing (Area, Options)
import PixelEngine.Graphics.Abstract as Abstract
import Task


{-| an alias for the program
-}
type alias PixelEngine flag model msg =
    Program flag (Model model msg) (Msg msg)


type alias Config msg =
    { windowSize : Maybe { width : Float, height : Float }
    , controls : ( String -> Input, Input -> msg )
    }


type alias Model model msg =
    { modelContent : model
    , config : Config msg
    }


type Msg msg
    = Resize { width : Float, height : Float }
    | MsgContent msg


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


subscriptionsFunction : (model -> Sub msg) -> Model model msg -> Sub (Msg msg)
subscriptionsFunction subscriptions ({ modelContent, config } as model) =
    Sub.batch
        [ subscriptions modelContent |> Sub.map MsgContent
        , Events.onResize <| \w h -> Resize { width = toFloat w, height = toFloat h }
        , Controls.basic (config.controls |> Tuple.second) |> Sub.map MsgContent
        ]


viewFunction : (model -> { title : String, options : Options msg, body : List (Area msg) }) -> Model model msg -> Browser.Document (Msg msg)
viewFunction view ({ modelContent, config } as model) =
    let
        { windowSize, controls } =
            config

        { title, options, body } =
            view modelContent

        (Abstract.Options { width, scale }) =
            options

        height =
            scale * Graphics.heightOf body
    in
    { title = title
    , body =
        [(case windowSize of
            Just wS ->
                Graphics.render
                    (options
                        |> Graphics.usingScale
                            (toFloat <|
                                min (2 ^ (floor <| logBase 2 <| ( wS.height) / height))
                                    (2 ^ (floor <| logBase 2 <| ( wS.width) / width))
                            )
                        |> Controls.supportingMobile { windowSize = wS, controls = controls |> Tuple.second }
                    )
                    body

            Nothing ->
                Graphics.render options []
        )
            |> Html.map MsgContent
        ]
    }


initFunction : ( String -> Input, Input -> msg ) -> (flags -> ( model, Cmd msg )) -> (flags -> ( Model model msg, Cmd (Msg msg) ))
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
            , Task.perform (\{viewport} -> let {width,height} = viewport in Resize { width = width, height = height }) Dom.getViewport
            ]
        )


{-| uses custom controls
-}
programWithCustomControls :
    { init : flags -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : model -> { title : String, options : Options msg, body : List (Area msg) }
    , controls : ( String -> Input, Input -> msg )
    }
    -> Program flags (Model model msg) (Msg msg)
programWithCustomControls { init, update, subscriptions, view, controls } =
    Browser.document
        { init =
            initFunction controls init
        , update =
            updateFunction update
        , subscriptions =
            subscriptionsFunction subscriptions
        , view =
            viewFunction view
        }


{-| use this function as usual
-}
program :
    { init : flags -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : model -> { title : String, options : Options msg, body : List (Area msg) }
    , controls : Input -> msg
    }
    -> Program flags (Model model msg) (Msg msg)
program { init, update, subscriptions, view, controls } =
    programWithCustomControls
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , controls = ( Controls.defaultLayout, controls )
        }
