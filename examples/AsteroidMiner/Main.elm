module AsteroidMiner.Main exposing (main)

import Action exposing (Action)
import AsteroidMiner.Building exposing (BuildingType(..))
import AsteroidMiner.Data exposing (fps, size, spriteSize)
import AsteroidMiner.Lib.Map exposing (SquareType(..))
import AsteroidMiner.Page as Page exposing (GameMode(..))
import AsteroidMiner.Page.Game as Game
import AsteroidMiner.Page.Menu as Menu
import AsteroidMiner.Page.Tutorial as Tutorial
import Location exposing (Angle(..))
import PixelEngine exposing (Area, Input(..), PixelEngine, gameWithNoControls)
import PixelEngine.Options as Options exposing (Options)
import Random exposing (Seed)



----------------------
-- Model
----------------------


type Model
    = Loading
    | Menu Menu.Model
    | Game Game.Model
    | Tutorial Tutorial.Model


type LoadingMsg
    = GotSeed Seed


type Msg
    = GameSpecific Game.Msg
    | TutorialSpecific Tutorial.Msg
    | MenuSpecific Menu.Msg
    | LoadingSpecific LoadingMsg


type alias LoadingAction =
    Action Never Never Seed Never



----------------------
-- Init
----------------------


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , Random.independentSeed |> Random.generate (LoadingSpecific << GotSeed)
    )



----------------------
-- Update
----------------------


updateLoading : LoadingMsg -> LoadingAction
updateLoading msg =
    case msg of
        GotSeed seed ->
            Action.transitioning seed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( LoadingSpecific loadingMsg, Loading ) ->
            updateLoading loadingMsg
                |> Action.config
                |> Action.withTransition Menu.init Menu never
                |> Action.apply

        ( MenuSpecific menuMsg, Menu menuModel ) ->
            Menu.update menuMsg menuModel
                |> Action.config
                |> Action.withCustomTransition
                    (\data ->
                        case data of
                            Page.Game seed ->
                                let
                                    ( m, _ ) =
                                        Game.init seed
                                in
                                ( Game m, Cmd.none )

                            Page.Tutorial seed ->
                                let
                                    ( m, _ ) =
                                        Tutorial.init 1 seed
                                in
                                ( Tutorial m, Cmd.none )
                    )
                |> Action.apply

        ( GameSpecific gameMsg, Game gameModel ) ->
            Game.update gameMsg gameModel
                |> Action.config
                |> Action.withExit (init ())
                |> Action.withUpdate Game never
                |> Action.apply

        ( TutorialSpecific tutorialMsg, Tutorial tutorialModel ) ->
            Tutorial.update tutorialMsg tutorialModel
                |> Action.config
                |> Action.withExit (init ())
                |> Action.withUpdate Tutorial never
                |> Action.apply

        _ ->
            ( model, Cmd.none )



----------------------
-- Subscriptions
----------------------


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Loading ->
            Sub.none

        Menu _ ->
            Sub.none

        Game gameModel ->
            gameModel
                |> Game.subscriptions
                |> Sub.map GameSpecific

        Tutorial tutorialModel ->
            tutorialModel
                |> Tutorial.subscriptions
                |> Sub.map TutorialSpecific



----------------------
-- View
----------------------


view :
    Model
    -> { title : String, options : Maybe (Options Msg), body : List (Area Msg) }
view model =
    let
        defaultOptions : Options msg
        defaultOptions =
            Options.default
                |> Options.withMovementSpeed (1 / fps)

        map : (a -> Msg) -> { options : Options msg, body : List (Area a) } -> { options : Options msg, body : List (Area Msg) }
        map mapper o =
            { options = o.options
            , body = o.body |> List.map (PixelEngine.mapArea mapper)
            }

        { options, body } =
            case model of
                Loading ->
                    { options = defaultOptions, body = [] }

                Menu menuModel ->
                    Menu.view defaultOptions menuModel
                        |> map MenuSpecific

                Game gameModel ->
                    Game.view GameSpecific defaultOptions gameModel

                Tutorial tutorialModel ->
                    Tutorial.view TutorialSpecific defaultOptions tutorialModel
    in
    { title = "Asteroid Miner"
    , options = Just options
    , body = body
    }


height : Float
height =
    (toFloat <| size) * spriteSize


main : PixelEngine () Model Msg
main =
    gameWithNoControls
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , width = height
        }
