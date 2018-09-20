module DigDigBoom.Main exposing (main)

import Browser exposing (Document)
import Css
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
import PixelEngine exposing (PixelEngine, program)
import PixelEngine.Controls as Controls exposing (Input(..))
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
updateGame fun ({ player, map, gameType } as modelContent) =
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
        Just ({ player, map, gameType } as modelContent) ->
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
subscriptions model =
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
        , background = Graphics.colorBackground (Css.rgb 20 12 28)
        , tileset = tileset
        }
        []
    , Graphics.tiledArea
        { rows = 2
        , background = Graphics.colorBackground (Css.rgb 20 12 28)
        , tileset = tileset
        }
        [ ( ( 4, 0 ), Tileset.letter_y Tileset.colorWhite )
        , ( ( 5, 0 ), Tileset.letter_o Tileset.colorWhite )
        , ( ( 6, 0 ), Tileset.letter_u Tileset.colorWhite )
        , ( ( 8, 0 ), Tileset.letter_h Tileset.colorWhite )
        , ( ( 9, 0 ), Tileset.letter_a Tileset.colorWhite )
        , ( ( 10, 0 ), Tileset.letter_v Tileset.colorWhite )
        , ( ( 11, 0 ), Tileset.letter_e Tileset.colorWhite )
        , ( ( 6, 1 ), Tileset.letter_d Tileset.colorWhite )
        , ( ( 7, 1 ), Tileset.letter_i Tileset.colorWhite )
        , ( ( 8, 1 ), Tileset.letter_e Tileset.colorWhite )
        , ( ( 9, 1 ), Tileset.letter_d Tileset.colorWhite )
        ]
    , Graphics.imageArea
        { height = toFloat <| 12 * 16
        , background = Graphics.colorBackground (Css.rgb 20 12 28)
        }
        [ ( ( toFloat <| (16 * width) // 2 - 64, toFloat <| (12 * width) // 2 - 64 ), image "skull.png" )
        ]
    , Graphics.tiledArea
        { rows = 2
        , background = Graphics.colorBackground (Css.rgb 20 12 28)
        , tileset = tileset
        }
        [ ( ( 4, 0 ), Tileset.letter_p Tileset.colorWhite )
        , ( ( 5, 0 ), Tileset.letter_r Tileset.colorWhite )
        , ( ( 6, 0 ), Tileset.letter_e Tileset.colorWhite )
        , ( ( 7, 0 ), Tileset.letter_s Tileset.colorWhite )
        , ( ( 8, 0 ), Tileset.letter_s Tileset.colorWhite )
        , ( ( 10, 0 ), Tileset.letter_a Tileset.colorWhite )
        , ( ( 11, 0 ), Tileset.letter_n Tileset.colorWhite )
        , ( ( 12, 0 ), Tileset.letter_y Tileset.colorWhite )
        , ( ( 6, 1 ), Tileset.letter_b Tileset.colorWhite )
        , ( ( 7, 1 ), Tileset.letter_u Tileset.colorWhite )
        , ( ( 8, 1 ), Tileset.letter_t Tileset.colorWhite )
        , ( ( 9, 1 ), Tileset.letter_t Tileset.colorWhite )
        , ( ( 10, 1 ), Tileset.letter_o Tileset.colorWhite )
        , ( ( 11, 1 ), Tileset.letter_n Tileset.colorWhite )
        ]
    , Graphics.tiledArea
        { rows = 2
        , background = Graphics.colorBackground (Css.rgb 20 12 28)
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
        , background = Graphics.colorBackground (Css.rgb 20 12 28)
        , tileset = tileset
        }
        []
    , Graphics.tiledArea
        { rows = 3
        , background = Graphics.colorBackground (Css.rgb 20 12 28)
        , tileset = tileset
        }
        [ ( ( 5, 0 ), Tileset.letter_d Tileset.colorWhite )
        , ( ( 6, 0 ), Tileset.letter_i Tileset.colorWhite )
        , ( ( 7, 0 ), Tileset.letter_g Tileset.colorWhite )
        , ( ( 6, 1 ), Tileset.letter_d Tileset.colorWhite )
        , ( ( 7, 1 ), Tileset.letter_i Tileset.colorWhite )
        , ( ( 8, 1 ), Tileset.letter_g Tileset.colorWhite )
        , ( ( 6, 2 ), Tileset.letter_b Tileset.colorWhite )
        , ( ( 7, 2 ), Tileset.letter_o Tileset.colorWhite )
        , ( ( 8, 2 ), Tileset.letter_o Tileset.colorWhite )
        , ( ( 9, 2 ), Tileset.letter_m Tileset.colorWhite )
        ]
    , Graphics.imageArea
        { height = toFloat <| 9 * 16
        , background = Graphics.colorBackground (Css.rgb 20 12 28)
        }
        [ ( ( toFloat <| (16 * width) // 2 - 64, 0 ), Image.fromTile tile logo )
        ]
    , Graphics.tiledArea
        { rows = 4
        , background = Graphics.colorBackground (Css.rgb 20 12 28)
        , tileset = tileset
        }
        [ ( ( 1, 0 ), Tileset.letter_a Tileset.colorWhite )
        , ( ( 2, 0 ), Tileset.arrow_left Tileset.colorWhite )
        , ( ( 3, 0 ), Tileset.arrow_right Tileset.colorWhite )
        , ( ( 4, 0 ), Tileset.letter_d Tileset.colorWhite )
        , ( ( 6, 0 ), Tileset.letter_minus Tileset.colorWhite )
        , ( ( 7, 0 ), Tileset.letter_s Tileset.colorWhite )
        , ( ( 8, 0 ), Tileset.letter_t Tileset.colorWhite )
        , ( ( 9, 0 ), Tileset.letter_a Tileset.colorWhite )
        , ( ( 10, 0 ), Tileset.letter_r Tileset.colorWhite )
        , ( ( 11, 0 ), Tileset.letter_t Tileset.colorWhite )
        , ( ( 8, 1 ), Tileset.letter_g Tileset.colorWhite )
        , ( ( 9, 1 ), Tileset.letter_a Tileset.colorWhite )
        , ( ( 10, 1 ), Tileset.letter_m Tileset.colorWhite )
        , ( ( 11, 1 ), Tileset.letter_e Tileset.colorWhite )
        , ( ( 1, 3 ), Tileset.letter_w Tileset.colorWhite )
        , ( ( 2, 3 ), Tileset.arrow_up Tileset.colorWhite )
        , ( ( 3, 3 ), Tileset.arrow_down Tileset.colorWhite )
        , ( ( 4, 3 ), Tileset.letter_s Tileset.colorWhite )
        , ( ( 6, 3 ), Tileset.letter_minus Tileset.colorWhite )
        , ( ( 7, 3 ), Tileset.letter_t Tileset.colorWhite )
        , ( ( 8, 3 ), Tileset.letter_u Tileset.colorWhite )
        , ( ( 9, 3 ), Tileset.letter_t Tileset.colorWhite )
        , ( ( 10, 3 ), Tileset.letter_o Tileset.colorWhite )
        , ( ( 11, 3 ), Tileset.letter_r Tileset.colorWhite )
        , ( ( 12, 3 ), Tileset.letter_i Tileset.colorWhite )
        , ( ( 13, 3 ), Tileset.letter_a Tileset.colorWhite )
        , ( ( 14, 3 ), Tileset.letter_l Tileset.colorWhite )
        ]
    , Graphics.tiledArea
        { rows = 2
        , background = Graphics.colorBackground (Css.rgb 20 12 28)
        , tileset = tileset
        }
        []
    ]


worldScreen : Int -> Map Cell -> PlayerData -> List ( Location, Tile msg ) -> List (Area msg)
worldScreen worldSeed map player hints =
    [ Graphics.tiledArea
        { rows = 1
        , background = Graphics.colorBackground (Css.rgb 20 12 28)
        , tileset = tileset
        }
        ([ ( ( 0, 0 ), Tileset.letter_x Tileset.colorWhite )
         , ( ( 1, 0 ), Tileset.letter_minus Tileset.colorWhite )
         , ( ( 2, 0 ), Tileset.letter_e Tileset.colorWhite )
         , ( ( 3, 0 ), Tileset.letter_x Tileset.colorWhite )
         , ( ( 4, 0 ), Tileset.letter_i Tileset.colorWhite )
         , ( ( 5, 0 ), Tileset.letter_t Tileset.colorWhite )
         , ( ( 7, 0 ), Tileset.letter_s Tileset.colorWhite )
         , ( ( 8, 0 ), Tileset.letter_c Tileset.colorWhite )
         , ( ( 9, 0 ), Tileset.letter_o Tileset.colorWhite )
         , ( ( 10, 0 ), Tileset.letter_r Tileset.colorWhite )
         , ( ( 11, 0 ), Tileset.letter_e Tileset.colorWhite )
         , ( ( 12, 0 ), Tileset.letter_colon Tileset.colorWhite )
         , ( ( 14, 0 ), Tileset.numberToTile (modBy 100 (abs worldSeed) // 10) Tileset.colorWhite )
         , ( ( 15, 0 ), Tileset.numberToTile (modBy 10 (abs worldSeed)) Tileset.colorWhite )
         ]
            |> (if (worldSeed // abs worldSeed) == -1 then
                    List.append [ ( ( 13, 0 ), Tileset.letter_minus Tileset.colorWhite ) ]

                else
                    List.append []
               )
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
        , background = Graphics.colorBackground (Css.rgb 20 12 28)
        , tileset = tileset
        }
        ([ ( ( 4, 2 ), Tileset.arrow_up Tileset.colorWhite )
         , ( ( 5, 2 ), Tileset.letter_s Tileset.colorWhite )
         , ( ( 6, 2 ), Tileset.letter_p Tileset.colorWhite )
         , ( ( 7, 2 ), Tileset.letter_a Tileset.colorWhite )
         , ( ( 8, 2 ), Tileset.letter_c Tileset.colorWhite )
         , ( ( 9, 2 ), Tileset.letter_e Tileset.colorWhite )
         , ( ( 10, 2 ), Tileset.letter_minus Tileset.colorWhite )
         , ( ( 11, 2 ), Tileset.letter_u Tileset.colorWhite )
         , ( ( 12, 2 ), Tileset.letter_s Tileset.colorWhite )
         , ( ( 13, 2 ), Tileset.letter_e Tileset.colorWhite )

         --
         , ( ( 0, 0 ), Tileset.arrow_down Tileset.colorWhite )
         , ( ( 1, 0 ), Tileset.letter_f Tileset.colorWhite )
         , ( ( 2, 0 ), Tileset.letter_l Tileset.colorWhite )
         , ( ( 3, 0 ), Tileset.letter_o Tileset.colorWhite )
         , ( ( 4, 0 ), Tileset.letter_o Tileset.colorWhite )
         , ( ( 5, 0 ), Tileset.letter_r Tileset.colorWhite )
         , ( ( 2, 1 ), Tileset.letter_q Tileset.colorWhite )
         , ( ( 3, 1 ), Tileset.arrow_left Tileset.colorWhite )
         , ( ( 12, 1 ), Tileset.arrow_right Tileset.colorWhite )
         , ( ( 13, 1 ), Tileset.letter_e Tileset.colorWhite )
         ]
            |> List.append
                (case player.inventory |> Inventory.ground of
                    Just a ->
                        [ ( ( 0, 1 ), Cell.getImage (Item a) ) ]

                    Nothing ->
                        []
                )
            |> List.append
                (List.range 0 (player.lifes - 1)
                    |> List.map (\i -> ( ( 15 - i, 0 ), Tileset.heart Tileset.colorRed ))
                )
            |> List.append
                (player.inventory
                    |> Inventory.get
                    |> List.indexedMap
                        (\i a ->
                            ( ( 4 + i, 1 ), Cell.getImage (Item a) )
                        )
                )
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
                                ([ ( ( 2, 4 ), Tileset.letter_h Tileset.colorWhite )
                                 , ( ( 3, 4 ), Tileset.letter_i Tileset.colorWhite )
                                 , ( ( 4, 4 ), Tileset.letter_n Tileset.colorWhite )
                                 , ( ( 5, 4 ), Tileset.letter_t Tileset.colorWhite )
                                 , ( ( 6, 4 ), Tileset.letter_colon Tileset.colorWhite )
                                 ]
                                    |> List.append
                                        (case num of
                                            5 ->
                                                [ ( ( 2, 5 ), Tileset.arrow_left Tileset.colorWhite )
                                                , ( ( 3, 5 ), Tileset.arrow_left Tileset.colorWhite )
                                                , ( ( 4, 5 ), Tileset.arrow_right Tileset.colorWhite )
                                                , ( ( 5, 5 ), Tileset.arrow_right Tileset.colorWhite )
                                                , ( ( 6, 5 ), Tileset.letter_exclamation_mark Tileset.colorWhite )
                                                , ( ( 7, 5 ), Tileset.arrow_down Tileset.colorWhite )
                                                , ( ( 8, 5 ), Tileset.arrow_down Tileset.colorWhite )
                                                , ( ( 9, 5 ), Tileset.arrow_right Tileset.colorWhite )
                                                , ( ( 10, 5 ), Tileset.letter_exclamation_mark Tileset.colorWhite )
                                                , ( ( 11, 5 ), Tileset.arrow_right Tileset.colorWhite )
                                                ]

                                            4 ->
                                                [ ( ( 2, 5 ), Tileset.arrow_down Tileset.colorWhite )
                                                , ( ( 3, 5 ), Tileset.arrow_right Tileset.colorWhite )
                                                , ( ( 4, 5 ), Tileset.arrow_right Tileset.colorWhite )
                                                , ( ( 5, 5 ), Tileset.letter_exclamation_mark Tileset.colorWhite )
                                                , ( ( 6, 5 ), Tileset.arrow_right Tileset.colorWhite )
                                                , ( ( 7, 5 ), Tileset.arrow_right Tileset.colorWhite )
                                                , ( ( 8, 5 ), Tileset.arrow_right Tileset.colorWhite )
                                                , ( ( 9, 5 ), Tileset.letter_exclamation_mark Tileset.colorWhite )
                                                , ( ( 10, 5 ), Tileset.arrow_right Tileset.colorWhite )
                                                ]

                                            3 ->
                                                [ ( ( 2, 5 ), Tileset.arrow_right Tileset.colorWhite )
                                                , ( ( 3, 5 ), Tileset.arrow_right Tileset.colorWhite )
                                                , ( ( 4, 5 ), Tileset.arrow_right Tileset.colorWhite )
                                                , ( ( 5, 5 ), Tileset.arrow_right Tileset.colorWhite )
                                                , ( ( 6, 5 ), Tileset.arrow_right Tileset.colorWhite )
                                                , ( ( 7, 5 ), Tileset.arrow_right Tileset.colorWhite )
                                                , ( ( 8, 5 ), Tileset.arrow_down Tileset.colorWhite )
                                                , ( ( 9, 5 ), Tileset.arrow_down Tileset.colorWhite )
                                                , ( ( 10, 5 ), Tileset.arrow_right Tileset.colorWhite )
                                                , ( ( 11, 5 ), Tileset.letter_exclamation_mark Tileset.colorWhite )
                                                ]

                                            2 ->
                                                [ ( ( 2, 5 ), Tileset.arrow_down Tileset.colorWhite )
                                                , ( ( 3, 5 ), Tileset.arrow_right Tileset.colorWhite )
                                                , ( ( 4, 5 ), Tileset.arrow_right Tileset.colorWhite )
                                                , ( ( 5, 5 ), Tileset.arrow_right Tileset.colorWhite )
                                                , ( ( 6, 5 ), Tileset.arrow_right Tileset.colorWhite )
                                                , ( ( 7, 5 ), Tileset.letter_exclamation_mark Tileset.colorWhite )
                                                ]

                                            _ ->
                                                [ ( ( 2, 5 ), Tileset.arrow_down Tileset.colorWhite )
                                                , ( ( 3, 5 ), Tileset.arrow_right Tileset.colorWhite )
                                                , ( ( 4, 5 ), Tileset.arrow_right Tileset.colorWhite )
                                                , ( ( 5, 5 ), Tileset.arrow_right Tileset.colorWhite )
                                                , ( ( 6, 5 ), Tileset.letter_exclamation_mark Tileset.colorWhite )
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
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , controls = Input
        }
