module PixelEngine exposing (PixelEngine, program, programWithCustomControls)

import Html.Styled as Html exposing (Html)
import PixelEngine.Controls as Controls exposing (Input)
import PixelEngine.Graphics as Graphics exposing (Area, Options)
import Task
import Window


type alias PixelEngine flag model msg =
    Program flag (Model model msg) (Msg msg)


type alias Config msg =
    { windowSize : Maybe Window.Size
    , controls : ( Char -> Input, Input -> msg )
    }


type alias Model model msg =
    { modelContent : model
    , config : Config msg
    }


type Msg msg
    = Resize Window.Size
    | MsgContent msg


batch : ( model, Cmd msg ) -> Model model msg -> ( Model model msg, Cmd (Msg msg) )
batch ( modelContent, msg ) { config } =
    ( { modelContent = modelContent, config = config }, msg |> Cmd.map MsgContent )


updateFunction : (msg -> model -> ( model, Cmd msg )) -> Msg msg -> Model model msg -> ( Model model msg, Cmd (Msg msg) )
updateFunction update msg ({ modelContent, config } as model) =
    case msg of
        Resize windowSize ->
            { model
                | config =
                    { config
                        | windowSize = Just windowSize
                    }
            }
                ! []

        MsgContent msg ->
            model |> batch (update msg modelContent)


subscriptionsFunction : (model -> Sub msg) -> Model model msg -> Sub (Msg msg)
subscriptionsFunction subscriptions ({ modelContent, config } as model) =
    Sub.batch
        [ subscriptions modelContent |> Sub.map MsgContent
        , Window.resizes Resize
        , Controls.basic (config.controls |> Tuple.second) |> Sub.map MsgContent
        ]


viewFunction : (model -> ( Options msg, List (Area msg) )) -> Model model msg -> Html (Msg msg)
viewFunction view ({ modelContent, config } as model) =
    let
        { windowSize, controls } =
            config

        ( options, listOfArea ) =
            view modelContent
    in
    (case windowSize of
        Just wS ->
            Graphics.render
                options
                --(options |> Controls.supportingMobile { windowSize = wS, controls = controls |> Tuple.second })
                listOfArea

        Nothing ->
            Graphics.render
                options
                []
    )
        |> Html.map MsgContent


initFunction : ( Char -> Input, Input -> msg ) -> ( model, Cmd msg ) -> ( Model model msg, Cmd (Msg msg) )
initFunction controls init =
    let
        ( modelContent, msg ) =
            init
    in
    { modelContent = modelContent
    , config = { windowSize = Nothing, controls = controls }
    }
        ! [ Task.perform Resize Window.size
          , msg |> Cmd.map MsgContent
          ]


programWithCustomControls :
    { init : ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : model -> ( Options msg, List (Area msg) )
    , controls : ( Char -> Input, Input -> msg )
    }
    -> Program Never (Model model msg) (Msg msg)
programWithCustomControls { init, update, subscriptions, view, controls } =
    Html.program
        { init =
            initFunction controls init
        , update =
            updateFunction update
        , subscriptions =
            subscriptionsFunction subscriptions
        , view =
            viewFunction view
        }


program :
    { init : ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : model -> ( Options msg, List (Area msg) )
    , controls : Input -> msg
    }
    -> Program Never (Model model msg) (Msg msg)
program { init, update, subscriptions, view, controls } =
    programWithCustomControls
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , controls = ( Controls.defaultLayout, controls )
        }
