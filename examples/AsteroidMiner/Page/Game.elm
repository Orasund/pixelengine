module AsteroidMiner.Page.Game exposing (Model, Msg, init, subscriptions, update, view)

import Action exposing (Action)
import AsteroidMiner.Data exposing (size, spriteSize, winAt)
import AsteroidMiner.Data.Map as Map exposing (GroundType(..), Map)
import AsteroidMiner.Lib.Map exposing (SquareType(..))
import AsteroidMiner.View.GUI as GUI
import AsteroidMiner.View.RunningGame as RunningGame exposing (Status(..))
import AsteroidMiner.View.Tileset as Tileset
import Color
import Grid.Bordered as Grid
import PixelEngine exposing (Area)
import PixelEngine.Image as Image
import PixelEngine.Options as Options exposing (Options, Transition)
import Random exposing (Seed)


type alias Model =
    RunningGame.Model


type alias GameAction =
    Action Model Never Never ()


init : Seed -> ( Model, Cmd msg )
init seed =
    ( RunningGame.init { map = Map.init, seed = seed, winCondition = winAt }, Cmd.none )


type Msg
    = Exit
    | GameSpecific RunningGame.Msg


subscriptions : Model -> Sub Msg
subscriptions =
    RunningGame.subscriptions
        >> Sub.map GameSpecific


update : Msg -> Model -> GameAction
update msg model =
    case msg of
        Exit ->
            Action.exiting

        GameSpecific gameMsg ->
            case model.status of
                Running ->
                    model
                        |> RunningGame.update gameMsg
                        >> (\m -> Action.updating ( m, Cmd.none ))

                _ ->
                    Action.updating ( model, Cmd.none )


areas : Model -> List (Area Msg)
areas ({ status } as model) =
    case status of
        Running ->
            model
                |> RunningGame.areas []
                |> List.map (PixelEngine.mapArea GameSpecific)

        Won ->
            [ PixelEngine.imageArea
                { height = (toFloat <| size) * spriteSize
                , background =
                    PixelEngine.colorBackground <|
                        Color.rgb255 218 212 94
                }
                [ ( ( (toFloat <| (size // 2) - 4) * spriteSize, (toFloat <| size // 2) * spriteSize )
                  , Image.fromText "Game Won" Tileset.font
                        |> Image.clickable Exit
                  )
                ]
            ]

        Lost ->
            [ PixelEngine.imageArea
                { height = (toFloat <| size) * spriteSize
                , background =
                    PixelEngine.colorBackground <|
                        Color.rgb255 20 12 28
                }
                [ ( ( (toFloat <| (size // 2) - 4) * spriteSize, (toFloat <| size // 2) * spriteSize )
                  , Image.fromText "Game Lost" Tileset.font
                        |> Image.clickable Exit
                  )
                ]
            ]


view :
    (Msg -> msg)
    -> Options msg
    -> Model
    -> { options : Options msg, body : List (Area msg) }
view mapper options model =
    let
        transition : Transition
        transition =
            Options.transition
                "win_transition"
                { start = "opacity:1;filter: blur(0px);"
                , keyFrames =
                    [ Just "opacity:1;filter: blur(0px);"
                    , Nothing
                    ]
                , end = "opacity:0;filter: blur(5px);"
                }
    in
    { options =
        options
            |> (case model.status of
                    Running ->
                        identity

                    _ ->
                        Options.withTransitionFrom
                            (model
                                |> RunningGame.areas []
                                |> List.map (PixelEngine.mapArea (GameSpecific >> mapper))
                            )
                            transition
               )
    , body = model |> areas |> List.map (PixelEngine.mapArea mapper)
    }
