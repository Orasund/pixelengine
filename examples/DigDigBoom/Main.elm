module DigDigBoom.Main exposing (main)

import DigDigBoom.Cell as Cell
    exposing
        ( Cell(..)
        , EnemyType(..)
        , ItemType(..)
        , SolidType(..)
        )
import DigDigBoom.Component.Map as Map exposing (Actor)
import DigDigBoom.Game as Game
import DigDigBoom.Player as Player exposing (PlayerData)
import DigDigBoom.View.Screen as Screen
import DigDigBoom.View.Tile as TileView
import DigDigBoom.View.Transition as Transition
import DigDigBoom.View.Tutorial as Tutorial
import Grid as Grid exposing (Grid)
import Grid.Direction exposing (Direction(..))
import PixelEngine exposing (Area, Input(..), PixelEngine, game)
import PixelEngine.Options as Options exposing (Options)
import Random



-------------------------------
-- MODEL
-------------------------------


type GameType
    = Rogue
        { seed : Random.Seed
        , worldSeed : Int
        }
    | Tutorial Int


type alias ModelContent =
    { map : Grid Cell
    , oldScreen : Maybe (List (Area Msg))
    , player : PlayerData
    , gameType : GameType
    }


type alias Model =
    Maybe ModelContent


type Msg
    = Input Input



-------------------------------
-- CONSTANTS
-------------------------------


worldSize : Int
worldSize =
    16



-------------------------------
-- INIT
-------------------------------


init : flag -> ( Model, Cmd Msg )
init _ =
    ( Nothing
    , Cmd.none
    )



-------------------------------
-- UPDATE
-------------------------------


tutorial : Int -> ModelContent
tutorial num =
    let
        backpackSize : Int
        backpackSize =
            8

        currentMap =
            Cell.tutorial num
                |> Grid.update ( 7, 7 ) (always (Just (Player Down)))
    in
    { map = currentMap
    , oldScreen = Nothing
    , player = Player.init backpackSize
    , gameType =
        Tutorial num
    }


createNewMap : Int -> ModelContent
createNewMap worldSeed =
    let
        backpackSize : Int
        backpackSize =
            8

        ( currentMap, currentSeed ) =
            Random.step
                (Map.generator worldSize Cell.generator)
                (Random.initialSeed worldSeed)
                |> Tuple.mapFirst
                    (Grid.update ( 7, 7 ) <| always <| Just <| Player Down)
    in
    { map = currentMap
    , oldScreen = Nothing
    , player = Player.init backpackSize
    , gameType =
        Rogue
            { worldSeed = worldSeed
            , seed = currentSeed
            }
    }


nextLevel : ModelContent -> ( Model, Cmd Msg )
nextLevel { gameType, map, player } =
    case gameType of
        Rogue { worldSeed } ->
            ( Just
                (createNewMap (worldSeed + 7)
                    |> (\newModel ->
                            { newModel
                                | oldScreen = Just (Screen.world worldSeed map player [])
                            }
                       )
                )
            , Cmd.none
            )

        Tutorial num ->
            if num == 5 then
                ( Nothing
                , Cmd.none
                )

            else
                ( Just
                    (tutorial (num + 1)
                        |> (\newModel ->
                                { newModel
                                    | oldScreen = Just (Screen.world num map player [])
                                }
                           )
                    )
                , Cmd.none
                )


updateGame : (Player.Game -> Player.Game) -> ModelContent -> ( Model, Cmd Msg )
updateGame fun ({ player, map } as modelContent) =
    ( player, map )
        |> fun
        |> (\( playerData, newMap ) ->
                ( Just
                    { modelContent
                        | player = playerData
                        , map = newMap
                        , oldScreen = Nothing
                    }
                , Cmd.none
                )
           )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Just ({ map, gameType } as modelContent) ->
            if
                Grid.find
                    (\_ cell ->
                        case cell of
                            Enemy _ _ ->
                                True

                            _ ->
                                False
                    )
                    map
                    == Nothing
            then
                nextLevel modelContent

            else
                case msg of
                    Input input ->
                        let
                            maybePlayer : Grid Cell -> Maybe Actor
                            maybePlayer currentMap =
                                currentMap
                                    |> Grid.find
                                        (\_ cell ->
                                            case cell of
                                                Player _ ->
                                                    True

                                                _ ->
                                                    False
                                        )
                                    |> Maybe.andThen
                                        (\( key, cell ) ->
                                            case cell of
                                                Player dir ->
                                                    Just ( key, dir )

                                                _ ->
                                                    Nothing
                                        )
                        in
                        case maybePlayer map of
                            Just playerCell ->
                                let
                                    updateDirection dir game =
                                        ( playerCell, game )
                                            |> Game.applyDirection (worldSize - 1) dir
                                            |> Tuple.second
                                in
                                case input of
                                    InputA ->
                                        modelContent
                                            |> updateGame (Player.activate playerCell)

                                    InputUp ->
                                        modelContent
                                            |> updateGame (updateDirection Up)

                                    InputLeft ->
                                        modelContent
                                            |> updateGame (updateDirection Left)

                                    InputDown ->
                                        modelContent
                                            |> updateGame (updateDirection Down)

                                    InputRight ->
                                        modelContent
                                            |> updateGame (updateDirection Right)

                                    InputX ->
                                        modelContent
                                            |> updateGame (Tuple.mapFirst Player.rotateLeft)

                                    InputY ->
                                        modelContent
                                            |> updateGame (Tuple.mapFirst Player.rotateRight)

                                    InputB ->
                                        ( Nothing
                                        , Cmd.none
                                        )

                            Nothing ->
                                case gameType of
                                    Rogue { worldSeed } ->
                                        ( Just
                                            (createNewMap (worldSeed - 2)
                                                |> (\newModel ->
                                                        { newModel
                                                            | oldScreen = Just Screen.death
                                                        }
                                                   )
                                            )
                                        , Cmd.none
                                        )

                                    Tutorial num ->
                                        ( Just
                                            (tutorial num
                                                |> (\newModel ->
                                                        { newModel
                                                            | oldScreen = Just Screen.death
                                                        }
                                                   )
                                            )
                                        , Cmd.none
                                        )

        Nothing ->
            case msg of
                Input InputLeft ->
                    ( Just
                        (createNewMap 1
                            |> (\newModel ->
                                    { newModel
                                        | oldScreen = Just Screen.menu
                                    }
                               )
                        )
                    , Cmd.none
                    )

                Input InputRight ->
                    ( Just
                        (createNewMap 1
                            |> (\newModel ->
                                    { newModel
                                        | oldScreen = Just Screen.menu
                                    }
                               )
                        )
                    , Cmd.none
                    )

                Input InputUp ->
                    ( Just
                        (tutorial 1
                            |> (\newModel ->
                                    { newModel
                                        | oldScreen = Just Screen.menu
                                    }
                               )
                        )
                    , Cmd.none
                    )

                Input InputDown ->
                    ( Just
                        (tutorial 1
                            |> (\newModel ->
                                    { newModel
                                        | oldScreen = Just Screen.menu
                                    }
                               )
                        )
                    , Cmd.none
                    )

                _ ->
                    ( model
                    , Cmd.none
                    )



-------------------------------
-- SUBSCRIPTIONS
-------------------------------


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-------------------------------
-- VIEW
-------------------------------


viewRogue : ModelContent -> Int -> ( Options Msg -> Options Msg, List (Area Msg) )
viewRogue { oldScreen, player, map } worldSeed =
    case oldScreen of
        Just justOldScreen ->
            ( Transition.nextLevel justOldScreen
            , Screen.world worldSeed map player []
            )

        Nothing ->
            if player.lifes > 0 then
                ( identity
                , Screen.world worldSeed map player []
                )

            else
                ( Transition.death (Screen.world worldSeed map player [])
                , Screen.death
                )


width : Int
width =
    16


view : Model -> { title : String, options : Maybe (Options Msg), body : List (Area Msg) }
view model =
    let
        ( optionFunction, body ) =
            case model of
                Just ({ gameType, oldScreen, player, map } as modelContent) ->
                    case gameType of
                        Rogue { worldSeed } ->
                            viewRogue modelContent worldSeed

                        Tutorial num ->
                            Tutorial.view oldScreen player map num

                Nothing ->
                    ( identity, Screen.menu )

        options =
            optionFunction Options.default
    in
    { title = "Dig Dig Boom"
    , options = Just options
    , body = body
    }



-------------------------------
-- MAIN
-------------------------------


main : PixelEngine {} Model Msg
main =
    game
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , controls = Input >> Just
        , width = toFloat <| TileView.tileset.spriteWidth * width
        }
