module DigDigBoom.Main exposing (main)

import Color
import Dict
import DigDigBoom.Cell as Cell
    exposing
        ( Cell(..)
        , EnemyType(..)
        , ItemType(..)
        , SolidType(..)
        )
import DigDigBoom.Component.Inventory as Inventory
import DigDigBoom.Component.Map as Map exposing (Actor, Direction(..), Location, Map)
import DigDigBoom.Game as Game
import DigDigBoom.Player as Player exposing (PlayerData)
import DigDigBoom.Tileset as Tileset
import PixelEngine exposing (PixelEngine, game)
import PixelEngine.Controls exposing (Input(..))
import PixelEngine.Graphics as Graphics exposing (Area, Options)
import PixelEngine.Graphics.Image as Image exposing (image)
import PixelEngine.Graphics.Tile as Tile exposing (Tile, Tileset)
import PixelEngine.Graphics.Transition as Transition
import Random


type GameType
    = Rogue
        { seed : Random.Seed
        , worldSeed : Int
        }
    | Tutorial Int


type alias ModelContent =
    { map : Map Cell
    , oldScreen : Maybe (List (Area Msg))
    , player : PlayerData
    , gameType : GameType
    }


type alias Model =
    Maybe ModelContent


type Msg
    = Input Input


worldSize : Int
worldSize =
    16


init : flag -> ( Model, Cmd Msg )
init _ =
    ( Nothing
    , Cmd.none
    )


tutorial : Int -> ModelContent
tutorial num =
    let
        backpackSize : Int
        backpackSize =
            8

        currentMap =
            Cell.tutorial num
                |> Dict.update ( 7, 7 ) (always (Just (Player Down)))
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
                |> Tuple.mapFirst (Dict.update ( 7, 7 ) (always (Just (Player Down))))
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
                                | oldScreen = Just (worldScreen worldSeed map player [])
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
                                    | oldScreen = Just (worldScreen num map player [])
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
                map
                    |> Dict.toList
                    |> List.filter
                        (\( _, cell ) ->
                            case cell of
                                Enemy _ _ ->
                                    True

                                _ ->
                                    False
                        )
                    |> List.isEmpty
            then
                nextLevel modelContent

            else
                case msg of
                    Input input ->
                        let
                            maybePlayer : Map Cell -> Maybe Actor
                            maybePlayer currentMap =
                                currentMap
                                    |> Map.getUnique
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

                                    InputNone ->
                                        ( model
                                        , Cmd.none
                                        )

                            Nothing ->
                                case gameType of
                                    Rogue { worldSeed } ->
                                        ( Just
                                            (createNewMap (worldSeed - 2)
                                                |> (\newModel ->
                                                        { newModel
                                                            | oldScreen = Just deathScreen
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
                                                            | oldScreen = Just deathScreen
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
                                        | oldScreen = Just menuScreen
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
                                        | oldScreen = Just menuScreen
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
                                        | oldScreen = Just menuScreen
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
                                        | oldScreen = Just menuScreen
                                    }
                               )
                        )
                    , Cmd.none
                    )

                _ ->
                    ( model
                    , Cmd.none
                    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


tileset : Tileset
tileset =
    Tile.tileset { source = "tileset.png", spriteHeight = 16, spriteWidth = 16 }


logo : Tileset
logo =
    Tile.tileset { source = "title_image.png", spriteHeight = 128, spriteWidth = 128 }


deathScreen : List (Area msg)
deathScreen =
    let
        width : Int
        width =
            16
    in
    [ Graphics.tiledArea
        { rows = 2
        , background = Graphics.colorBackground (Color.rgb255 20 12 28)
        , tileset = tileset
        }
        []
    , Graphics.tiledArea
        { rows = 2
        , background = Graphics.colorBackground (Color.rgb255 20 12 28)
        , tileset = tileset
        }
        ( List.concat
            [( 4, 0 ) |> Tileset.text "You have" Tileset.colorWhite
            ,(( 6, 1 ) |> Tileset.text "died" Tileset.colorWhite)]
        )
    , Graphics.imageArea
        { height = toFloat <| 12 * 16
        , background = Graphics.colorBackground (Color.rgb255 20 12 28)
        }
        [ ( ( toFloat <| (16 * width) // 2 - 64, toFloat <| (12 * width) // 2 - 64 ), image "skull.png" )
        ]
    , Graphics.tiledArea
        { rows = 2
        , background = Graphics.colorBackground (Color.rgb255 20 12 28)
        , tileset = tileset
        }
        ( List.concat
            [ (4,0) |> Tileset.text "Press any" Tileset.colorWhite
            , ( 6, 1 ) |> Tileset.text "button" Tileset.colorWhite
            ]
        )
    , Graphics.tiledArea
        { rows = 2
        , background = Graphics.colorBackground (Color.rgb255 20 12 28)
        , tileset = tileset
        }
        []
    ]


menuScreen : List (Area msg)
menuScreen =
    let
        width : Int
        width =
            16

        tile : Tile msg
        tile =
            Tile.tile ( 0, 0 ) |> Tile.animated 1
    in
    [ Graphics.tiledArea
        { rows = 2
        , background = Graphics.colorBackground (Color.rgb255 20 12 28)
        , tileset = tileset
        }
        []
    , Graphics.tiledArea
        { rows = 3
        , background = Graphics.colorBackground (Color.rgb255 20 12 28)
        , tileset = tileset
        }
        (List.concat
            [ ( 5, 0 ) |> Tileset.text "DIG" Tileset.colorWhite
            , ( 6, 1 ) |> Tileset.text "DIG" Tileset.colorWhite
            , ( 6, 2 ) |> Tileset.text "BOOM" Tileset.colorWhite
            ]
        )
    , Graphics.imageArea
        { height = toFloat <| 9 * 16
        , background = Graphics.colorBackground (Color.rgb255 20 12 28)
        }
        [ ( ( toFloat <| (16 * width) // 2 - 64, 0 ), Image.fromTile tile logo )
        ]
    , Graphics.tiledArea
        { rows = 4
        , background = Graphics.colorBackground (Color.rgb255 20 12 28)
        , tileset = tileset
        }
        (List.concat
            [ ( 1, 0 ) |> Tileset.text "a" Tileset.colorWhite
            , [ ( ( 2, 0 ), Tileset.arrow_left Tileset.colorWhite )
              , ( ( 3, 0 ), Tileset.arrow_right Tileset.colorWhite )
              ]
            , ( 4, 0 ) |> Tileset.text "d -Start" Tileset.colorWhite
            , ( 8, 1 ) |> Tileset.text "Game" Tileset.colorWhite
            , ( 1, 3 ) |> Tileset.text "w" Tileset.colorWhite
            , [ ( ( 2, 3 ), Tileset.arrow_up Tileset.colorWhite )
              , ( ( 3, 3 ), Tileset.arrow_down Tileset.colorWhite )
              ]
            , ( 4, 3 ) |> Tileset.text "s -Tutorial" Tileset.colorWhite
            ]
        )
    , Graphics.tiledArea
        { rows = 2
        , background = Graphics.colorBackground (Color.rgb255 20 12 28)
        , tileset = tileset
        }
        []
    ]


worldScreen : Int -> Map Cell -> PlayerData -> List ( Location, Tile msg ) -> List (Area msg)
worldScreen worldSeed map player hints =
    [ Graphics.tiledArea
        { rows = 1
        , background = Graphics.colorBackground (Color.rgb255 20 12 28)
        , tileset = tileset
        }
        (( 0, 0 )
            |> Tileset.text
                ("X-exit score:"
                    ++ (if (worldSeed // abs worldSeed) == -1 then
                        "-"

                       else
                        " "
                      )
                    ++ (String.fromInt (modBy 100 (abs worldSeed) // 10))
                    ++ (String.fromInt (modBy 10 (abs worldSeed)))
                )
                Tileset.colorWhite
        )
    , Graphics.tiledArea
        { rows = 16
        , background = Graphics.imageBackground { source = "groundTile.png", width = 16, height = 16 }
        , tileset = tileset
        }
        (hints
            |> List.append
                (map
                    |> Dict.foldl
                        (\pos cell list ->
                            ( pos
                            , Cell.getImage cell
                            )
                                :: list
                        )
                        []
                )
        )
    , Graphics.tiledArea
        { rows = 3
        , background = Graphics.colorBackground (Color.rgb255 20 12 28)
        , tileset = tileset
        }
        (List.concat
            [ [ ( ( 4, 2 ), Tileset.arrow_up Tileset.colorWhite ) ]
            , ( 5, 2 ) |> Tileset.text "SPACE-use" Tileset.colorWhite
            , [ ( ( 0, 0 ), Tileset.arrow_down Tileset.colorWhite ) ]
            , ( 1, 0 ) |> Tileset.text "floor" Tileset.colorWhite
            , ( 2, 1 ) |> Tileset.text "Q" Tileset.colorWhite
            , [ ( ( 3, 1 ), Tileset.arrow_left Tileset.colorWhite ) ]
            , [ ( ( 12, 1 ), Tileset.arrow_right Tileset.colorWhite ) ]
            , ( 13, 1 ) |> Tileset.text "E" Tileset.colorWhite
            , case player.inventory |> Inventory.ground of
                Just a ->
                    [ ( ( 0, 1 ), Cell.getImage (Item a) ) ]

                Nothing ->
                    []
            , List.range 0 (player.lifes - 1)
                |> List.map (\i -> ( ( 15 - i, 0 ), Tileset.heart Tileset.colorRed ))
            , player.inventory
                |> Inventory.get
                |> List.indexedMap
                    (\i a ->
                        ( ( 4 + i, 1 ), Cell.getImage (Item a) )
                    )
            ]
        )
    ]


view : Model -> { title : String, options : Options Msg, body : List (Area Msg) }
view model =
    let
        width : Int
        width =
            16

        options =
            Graphics.options
                { width = toFloat <| tileset.spriteWidth * width
                , transitionSpeedInSec = 0.2
                }

        title : String
        title =
            "Dig Dig Boom"
    in
    case model of
        Just { oldScreen, gameType, player, map } ->
            case gameType of
                Rogue { worldSeed } ->
                    case oldScreen of
                        Just justOldScreen ->
                            { title = title
                            , options =
                                options
                                    |> Transition.from justOldScreen
                                        (Transition.custom
                                            "next_level"
                                            [ ( 0, "filter:saturate(200%) contrast(100%);overflow:hidden;width:100%" ) --(toString <| scale * tileset.spriteWidth * width)
                                            , ( 2, "filter:saturate(50%) contrast(150%);overflow:hidden;width:0%;" )
                                            ]
                                        )
                            , body = worldScreen worldSeed map player []
                            }

                        Nothing ->
                            if player.lifes > 0 then
                                { title = title
                                , options = options
                                , body = worldScreen worldSeed map player []
                                }

                            else
                                { title = title
                                , options =
                                    options
                                        |> Transition.from
                                            (worldScreen
                                                worldSeed
                                                map
                                                player
                                                []
                                            )
                                            (Transition.custom
                                                "death_transition"
                                                [ ( 0, "opacity:1;filter:grayscale(0%) blur(0px);" )
                                                , ( 1, "opacity:1;filter:grayscale(70%) blur(0px);" )
                                                , ( 3, "opacity:0;filter:grayscale(70%) blur(5px);" )
                                                ]
                                            )
                                , body = deathScreen
                                }

                Tutorial num ->
                    let
                        tutorialWorldScreen =
                            worldScreen num
                                map
                                player
                                (( 2, 4 )
                                    |> Tileset.text "hint:" Tileset.colorWhite
                                    |> List.append
                                        (case num of
                                            5 ->
                                                List.concat
                                                    [ [ ( ( 2, 5 ), Tileset.arrow_left Tileset.colorWhite )
                                                      , ( ( 3, 5 ), Tileset.arrow_left Tileset.colorWhite )
                                                      , ( ( 4, 5 ), Tileset.arrow_right Tileset.colorWhite )
                                                      , ( ( 5, 5 ), Tileset.arrow_right Tileset.colorWhite )
                                                      ]
                                                    , ( 6, 5 ) |> Tileset.text "!" Tileset.colorWhite
                                                    , [ ( ( 7, 5 ), Tileset.arrow_down Tileset.colorWhite )
                                                      , ( ( 8, 5 ), Tileset.arrow_down Tileset.colorWhite )
                                                      , ( ( 9, 5 ), Tileset.arrow_right Tileset.colorWhite )
                                                      ]
                                                    , ( 10, 5 ) |> Tileset.text "!" Tileset.colorWhite
                                                    , [ ( ( 11, 5 ), Tileset.arrow_right Tileset.colorWhite )
                                                      ]
                                                    ]

                                            4 ->
                                                List.concat
                                                    [ [ ( ( 2, 5 ), Tileset.arrow_down Tileset.colorWhite )
                                                      , ( ( 3, 5 ), Tileset.arrow_right Tileset.colorWhite )
                                                      , ( ( 4, 5 ), Tileset.arrow_right Tileset.colorWhite )
                                                      ]
                                                    , ( 5, 5 ) |> Tileset.text "!" Tileset.colorWhite
                                                    , [ ( ( 6, 5 ), Tileset.arrow_right Tileset.colorWhite )
                                                      , ( ( 7, 5 ), Tileset.arrow_right Tileset.colorWhite )
                                                      , ( ( 8, 5 ), Tileset.arrow_right Tileset.colorWhite )
                                                      ]
                                                    , ( 9, 5 ) |> Tileset.text "!" Tileset.colorWhite
                                                    , [ ( ( 10, 5 ), Tileset.arrow_right Tileset.colorWhite ) ]
                                                    ]

                                            3 ->
                                                List.concat
                                                    [ [ ( ( 2, 5 ), Tileset.arrow_right Tileset.colorWhite )
                                                      , ( ( 3, 5 ), Tileset.arrow_right Tileset.colorWhite )
                                                      , ( ( 4, 5 ), Tileset.arrow_right Tileset.colorWhite )
                                                      , ( ( 5, 5 ), Tileset.arrow_right Tileset.colorWhite )
                                                      , ( ( 6, 5 ), Tileset.arrow_right Tileset.colorWhite )
                                                      , ( ( 7, 5 ), Tileset.arrow_right Tileset.colorWhite )
                                                      , ( ( 8, 5 ), Tileset.arrow_down Tileset.colorWhite )
                                                      , ( ( 9, 5 ), Tileset.arrow_down Tileset.colorWhite )
                                                      , ( ( 10, 5 ), Tileset.arrow_right Tileset.colorWhite )
                                                      ]
                                                    , ( 11, 5 ) |> Tileset.text "!" Tileset.colorWhite
                                                    ]

                                            2 ->
                                                List.concat
                                                    [ [ ( ( 2, 5 ), Tileset.arrow_down Tileset.colorWhite )
                                                      , ( ( 3, 5 ), Tileset.arrow_right Tileset.colorWhite )
                                                      , ( ( 4, 5 ), Tileset.arrow_right Tileset.colorWhite )
                                                      , ( ( 5, 5 ), Tileset.arrow_right Tileset.colorWhite )
                                                      , ( ( 6, 5 ), Tileset.arrow_right Tileset.colorWhite )
                                                      ]
                                                    , ( 7, 5 ) |> Tileset.text "!" Tileset.colorWhite
                                                    ]

                                            _ ->
                                                List.concat
                                                    [ [ ( ( 2, 5 ), Tileset.arrow_down Tileset.colorWhite )
                                                      , ( ( 3, 5 ), Tileset.arrow_right Tileset.colorWhite )
                                                      , ( ( 4, 5 ), Tileset.arrow_right Tileset.colorWhite )
                                                      , ( ( 5, 5 ), Tileset.arrow_right Tileset.colorWhite )
                                                      ]
                                                    , ( 6, 5 ) |> Tileset.text "!" Tileset.colorWhite
                                                    ]
                                        )
                                )
                    in
                    case oldScreen of
                        Just justOldScreen ->
                            { title = title
                            , options =
                                options
                                    |> Transition.from justOldScreen
                                        (Transition.custom
                                            "next_level"
                                            [ ( 0, "filter:saturate(200%) contrast(100%);overflow:hidden;width:100%;" ) --(toString <| scale * tileset.spriteWidth * width)
                                            , ( 2, "filter:saturate(50%) contrast(150%);overflow:hidden;width:0%;" )
                                            ]
                                        )
                            , body = tutorialWorldScreen
                            }

                        Nothing ->
                            if player.lifes > 0 then
                                { title = title
                                , options = options
                                , body = tutorialWorldScreen
                                }

                            else
                                { title = title
                                , options =
                                    options
                                        |> Transition.from tutorialWorldScreen
                                            (Transition.custom
                                                "death_transition"
                                                [ ( 0, "opacity:1;filter:grayscale(0%) blur(0px);" )
                                                , ( 1, "opacity:1;filter:grayscale(70%) blur(0px);" )
                                                , ( 3, "opacity:0;filter:grayscale(70%) blur(5px);" )
                                                ]
                                            )
                                , body = deathScreen
                                }

        Nothing ->
            { title = title
            , options = options
            , body = menuScreen
            }


main : PixelEngine {} Model Msg
main =
    game
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , controls = Input
        }
