module DigDigBoom.Main exposing (main)

import Char
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
import DigDigBoom.Component.Map as Map exposing (Direction(..), Location, Map)
import DigDigBoom.Game as Game
import DigDigBoom.Player as Player exposing (PlayerCell, PlayerData)
import DigDigBoom.Tileset as Tileset
import Html.Styled exposing (Html, program)
import Keyboard
import PixelEngine.Graphics as Graphics exposing (Area)
import PixelEngine.Graphics.Image as Image exposing (image)
import PixelEngine.Graphics.Tile as Tile exposing (Tile, Tileset)
import PixelEngine.ScreenTransition as Transition
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


type Input
    = Direction Direction
    | Activate
    | RotateLeft
    | RotateRight


type Msg
    = Input Input
    | NextLevel
    | Idle
    | Return


worldSize : Int
worldSize =
    16


init : ( Model, Cmd Msg )
init =
    Nothing ! [ Cmd.none ]



{- { map = currentMap
   , oldScreen = Nothing
   , player = Player.init backpackSize
   , gameType =
       Rogue
           { worldSeed = worldSeed
           , seed = currentSeed
           }
   }
       ! [ Cmd.none ]
-}


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


newMap : Int -> ModelContent
newMap worldSeed =
    let
        backpackSize : Int
        backpackSize =
            8

        ( currentMap, currentSeed ) =
            Map.generate
                (worldSize - 1)
                Cell.mapGenerator
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Just ({ player, map, gameType } as modelContent) ->
            case msg of
                Input input ->
                    let
                        maybePlayer : Map Cell -> Maybe PlayerCell
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
                            ( player, map )
                                |> (case input of
                                        Activate ->
                                            Player.activate playerCell

                                        Direction dir ->
                                            \game -> ( playerCell, game ) |> Game.applyDirection (worldSize - 1) dir |> Tuple.second

                                        RotateLeft ->
                                            Tuple.mapFirst Player.rotateLeft

                                        RotateRight ->
                                            Tuple.mapFirst Player.rotateRight
                                   )
                                |> (\( playerData, newMap ) ->
                                        Just
                                            { modelContent
                                                | player = playerData
                                                , map = newMap
                                                , oldScreen = Nothing
                                            }
                                            ! [ Cmd.none ]
                                   )

                        Nothing ->
                            case gameType of
                                Rogue { worldSeed } ->
                                    Just
                                        (newMap (worldSeed - 1)
                                            |> (\newModel ->
                                                    { newModel
                                                        | oldScreen = Just deathScreen
                                                    }
                                               )
                                        )
                                        ! [ Cmd.none ]

                                Tutorial num ->
                                    Just
                                        (tutorial num
                                            |> (\newModel ->
                                                    { newModel
                                                        | oldScreen = Just deathScreen
                                                    }
                                               )
                                        )
                                        ! [ Cmd.none ]

                NextLevel ->
                    case gameType of
                        Rogue { worldSeed } ->
                            Just
                                (newMap (worldSeed + 7)
                                    |> (\newModel ->
                                            { newModel
                                                | oldScreen = Just (worldScreen worldSeed map player [])
                                            }
                                       )
                                )
                                ! [ Cmd.none ]

                        Tutorial num ->
                            if num == 5 then
                                Nothing ! [ Cmd.none ]
                            else
                                Just
                                    (tutorial (num + 1)
                                        |> (\newModel ->
                                                { newModel
                                                    | oldScreen = Just (worldScreen num map player [])
                                                }
                                           )
                                    )
                                    ! [ Cmd.none ]

                Return ->
                    Nothing ! [ Cmd.none ]

                Idle ->
                    model ! [ Cmd.none ]

        Nothing ->
            case msg of
                Input (Direction Left) ->
                    Just
                        (newMap 0
                            |> (\newModel ->
                                    { newModel
                                        | oldScreen = Just menuScreen
                                    }
                               )
                        )
                        ! [ Cmd.none ]

                Input (Direction Right) ->
                    Just
                        (newMap 0
                            |> (\newModel ->
                                    { newModel
                                        | oldScreen = Just menuScreen
                                    }
                               )
                        )
                        ! [ Cmd.none ]

                Input (Direction Up) ->
                    Just
                        (tutorial 1
                            |> (\newModel ->
                                    { newModel
                                        | oldScreen = Just menuScreen
                                    }
                               )
                        )
                        ! [ Cmd.none ]

                Input (Direction Down) ->
                    Just
                        (tutorial 1
                            |> (\newModel ->
                                    { newModel
                                        | oldScreen = Just menuScreen
                                    }
                               )
                        )
                        ! [ Cmd.none ]

                _ ->
                    model ! [ Cmd.none ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.presses <|
        Char.fromCode
            >> (\char ->
                    case model of
                        Just { map } ->
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
                                NextLevel
                            else
                                case char of
                                    'w' ->
                                        Input (Direction Up)

                                    's' ->
                                        Input (Direction Down)

                                    'd' ->
                                        Input (Direction Right)

                                    'a' ->
                                        Input (Direction Left)

                                    ' ' ->
                                        Input Activate

                                    'q' ->
                                        Input RotateLeft

                                    'e' ->
                                        Input RotateRight

                                    'x' ->
                                        Return

                                    _ ->
                                        Idle

                        Nothing ->
                            case char of
                                'w' ->
                                    Input (Direction Up)

                                's' ->
                                    Input (Direction Down)

                                'd' ->
                                    Input (Direction Right)

                                'a' ->
                                    Input (Direction Left)

                                _ ->
                                    Idle
               )


tileset : Tileset
tileset =
    Tile.tileset { source = "tileset.png", spriteHeight = 16, spriteWidth = 16 }


logo : Tileset
logo =
    Tile.tileset { source = "title_image.png", spriteHeight = 128, spriteWidth = 128 }


deathScreen : List (Area msg)
deathScreen =
    let
        scale : Int
        scale =
            2

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
        [ ( ( 4, 0 ), Tileset.letter_y )
        , ( ( 5, 0 ), Tileset.letter_o )
        , ( ( 6, 0 ), Tileset.letter_u )
        , ( ( 8, 0 ), Tileset.letter_h )
        , ( ( 9, 0 ), Tileset.letter_a )
        , ( ( 10, 0 ), Tileset.letter_v )
        , ( ( 11, 0 ), Tileset.letter_e )
        , ( ( 6, 1 ), Tileset.letter_d )
        , ( ( 7, 1 ), Tileset.letter_i )
        , ( ( 8, 1 ), Tileset.letter_e )
        , ( ( 9, 1 ), Tileset.letter_d )
        ]
    , Graphics.imageArea
        { height = toFloat <| scale * 12 * 16
        , background = Graphics.colorBackground (Css.rgb 20 12 28)
        }
        [ ( ( toFloat <| (scale * 16 * width) // 2 - 128, toFloat <| (scale * 12 * width) // 2 - 128 ), image "skull.png" )
        ]
    , Graphics.tiledArea
        { rows = 2
        , background = Graphics.colorBackground (Css.rgb 20 12 28)
        , tileset = tileset
        }
        [ ( ( 4, 0 ), Tileset.letter_p )
        , ( ( 5, 0 ), Tileset.letter_r )
        , ( ( 6, 0 ), Tileset.letter_e )
        , ( ( 7, 0 ), Tileset.letter_s )
        , ( ( 8, 0 ), Tileset.letter_s )
        , ( ( 10, 0 ), Tileset.letter_a )
        , ( ( 11, 0 ), Tileset.letter_n )
        , ( ( 12, 0 ), Tileset.letter_y )
        , ( ( 6, 1 ), Tileset.letter_b )
        , ( ( 7, 1 ), Tileset.letter_u )
        , ( ( 8, 1 ), Tileset.letter_t )
        , ( ( 9, 1 ), Tileset.letter_t )
        , ( ( 10, 1 ), Tileset.letter_o )
        , ( ( 11, 1 ), Tileset.letter_n )
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
        scale : Int
        scale =
            2

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
        [ ( ( 5, 0 ), Tileset.letter_d )
        , ( ( 6, 0 ), Tileset.letter_i )
        , ( ( 7, 0 ), Tileset.letter_g )
        , ( ( 6, 1 ), Tileset.letter_d )
        , ( ( 7, 1 ), Tileset.letter_i )
        , ( ( 8, 1 ), Tileset.letter_g )
        , ( ( 6, 2 ), Tileset.letter_b )
        , ( ( 7, 2 ), Tileset.letter_o )
        , ( ( 8, 2 ), Tileset.letter_o )
        , ( ( 9, 2 ), Tileset.letter_m )
        ]
    , Graphics.imageArea
        { height = toFloat <| scale * 9 * 16
        , background = Graphics.colorBackground (Css.rgb 20 12 28)
        }
        [ ( ( toFloat <| (scale * 16 * width) // 2 - 128, 0 ), Image.fromTile tile logo )
        ]
    , Graphics.tiledArea
        { rows = 4
        , background = Graphics.colorBackground (Css.rgb 20 12 28)
        , tileset = tileset
        }
        [ ( ( 1, 0 ), Tileset.letter_a )
        , ( ( 2, 0 ), Tileset.arrow_left )
        , ( ( 3, 0 ), Tileset.arrow_right )
        , ( ( 4, 0 ), Tileset.letter_d )
        , ( ( 6, 0 ), Tileset.letter_minus )
        , ( ( 7, 0 ), Tileset.letter_s )
        , ( ( 8, 0 ), Tileset.letter_t )
        , ( ( 9, 0 ), Tileset.letter_a )
        , ( ( 10, 0 ), Tileset.letter_r )
        , ( ( 11, 0 ), Tileset.letter_t )
        , ( ( 8, 1 ), Tileset.letter_g )
        , ( ( 9, 1 ), Tileset.letter_a )
        , ( ( 10, 1 ), Tileset.letter_m )
        , ( ( 11, 1 ), Tileset.letter_e )
        , ( ( 1, 3 ), Tileset.letter_w )
        , ( ( 2, 3 ), Tileset.arrow_up )
        , ( ( 3, 3 ), Tileset.arrow_down )
        , ( ( 4, 3 ), Tileset.letter_s )
        , ( ( 6, 3 ), Tileset.letter_minus )
        , ( ( 7, 3 ), Tileset.letter_t )
        , ( ( 8, 3 ), Tileset.letter_u )
        , ( ( 9, 3 ), Tileset.letter_t )
        , ( ( 10, 3 ), Tileset.letter_o )
        , ( ( 11, 3 ), Tileset.letter_r )
        , ( ( 12, 3 ), Tileset.letter_i )
        , ( ( 13, 3 ), Tileset.letter_a )
        , ( ( 14, 3 ), Tileset.letter_l )
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
        ([ ( ( 0, 0 ), Tileset.letter_x )
         , ( ( 1, 0 ), Tileset.letter_minus )
         , ( ( 2, 0 ), Tileset.letter_e )
         , ( ( 3, 0 ), Tileset.letter_x )
         , ( ( 4, 0 ), Tileset.letter_i )
         , ( ( 5, 0 ), Tileset.letter_t )
         , ( ( 7, 0 ), Tileset.letter_s )
         , ( ( 8, 0 ), Tileset.letter_c )
         , ( ( 9, 0 ), Tileset.letter_o )
         , ( ( 10, 0 ), Tileset.letter_r )
         , ( ( 11, 0 ), Tileset.letter_e )
         , ( ( 12, 0 ), Tileset.letter_colon )
         , ( ( 14, 0 ), Tileset.numberToTile ((abs worldSeed % 100) // 10) )
         , ( ( 15, 0 ), Tileset.numberToTile (abs worldSeed % 10) )
         ]
            |> (if (worldSeed // abs worldSeed) == -1 then
                    List.append [ ( ( 13, 0 ), Tileset.letter_minus ) ]
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
        ([ ( ( 4, 2 ), Tileset.arrow_up )
         , ( ( 5, 2 ), Tileset.letter_s )
         , ( ( 6, 2 ), Tileset.letter_p )
         , ( ( 7, 2 ), Tileset.letter_a )
         , ( ( 8, 2 ), Tileset.letter_c )
         , ( ( 9, 2 ), Tileset.letter_e )
         , ( ( 10, 2 ), Tileset.letter_minus )
         , ( ( 11, 2 ), Tileset.letter_u )
         , ( ( 12, 2 ), Tileset.letter_s )
         , ( ( 13, 2 ), Tileset.letter_e )

         --
         , ( ( 0, 0 ), Tileset.arrow_down )
         , ( ( 1, 0 ), Tileset.letter_f )
         , ( ( 2, 0 ), Tileset.letter_l )
         , ( ( 3, 0 ), Tileset.letter_o )
         , ( ( 4, 0 ), Tileset.letter_o )
         , ( ( 5, 0 ), Tileset.letter_r )
         , ( ( 2, 1 ), Tileset.letter_q )
         , ( ( 3, 1 ), Tileset.arrow_left )
         , ( ( 12, 1 ), Tileset.arrow_right )
         , ( ( 13, 1 ), Tileset.letter_e )
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
                    |> List.map (\i -> ( ( 15 - i, 0 ), Tileset.heart ))
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


view : Model -> Html Msg
view model =
    let
        scale : Int
        scale =
            2

        width : Int
        width =
            16

        options =
            { scale = toFloat <| scale
            , width = toFloat <| scale * tileset.spriteWidth * width
            , transitionSpeedInSec = 0.2
            }
    in
    case model of
        Just { oldScreen, gameType, player, map } ->
            case gameType of
                Rogue { worldSeed } ->
                    case oldScreen of
                        Just justOldScreen ->
                            Transition.customTransition
                                "next_level"
                                [ ( 0, "overflow:hidden;width:" ++ (toString <| scale * tileset.spriteWidth * width) ++ "px;" )
                                , ( 2, "overflow:hidden;width:0px;" )
                                ]
                                |> Transition.apply
                                    options
                                    { from = justOldScreen
                                    , to = worldScreen worldSeed map player []
                                    }

                        Nothing ->
                            if player.lifes > 0 then
                                Graphics.render options (worldScreen worldSeed map player [])
                            else
                                Transition.customTransition
                                    "death_transition"
                                    [ ( 0, "opacity:1;filter:grayscale(0%) blur(0px);" )
                                    , ( 1, "opacity:1;filter:grayscale(70%) blur(0px);" )
                                    , ( 3, "opacity:0;filter:grayscale(70%) blur(5px);" )
                                    ]
                                    |> Transition.apply
                                        options
                                        { from = worldScreen worldSeed map player []
                                        , to = deathScreen
                                        }

                Tutorial num ->
                    let
                        tutorialWorldScreen =
                            worldScreen num
                                map
                                player
                                ([ ( ( 2, 4 ), Tileset.letter_h )
                                 , ( ( 3, 4 ), Tileset.letter_i )
                                 , ( ( 4, 4 ), Tileset.letter_n )
                                 , ( ( 5, 4 ), Tileset.letter_t )
                                 , ( ( 6, 4 ), Tileset.letter_colon )
                                 ]
                                    |> List.append
                                        (case num of
                                            5 ->
                                                [ ( ( 2, 5 ), Tileset.arrow_left )
                                                , ( ( 3, 5 ), Tileset.arrow_left )
                                                , ( ( 4, 5 ), Tileset.arrow_right )
                                                , ( ( 5, 5 ), Tileset.arrow_right )
                                                , ( ( 6, 5 ), Tileset.letter_exclamation_mark )
                                                , ( ( 7, 5 ), Tileset.arrow_down )
                                                , ( ( 8, 5 ), Tileset.arrow_down )
                                                , ( ( 9, 5 ), Tileset.arrow_right )
                                                , ( ( 10, 5 ), Tileset.letter_exclamation_mark )
                                                , ( ( 11, 5 ), Tileset.arrow_right )
                                                ]

                                            4 ->
                                                [ ( ( 2, 5 ), Tileset.arrow_down )
                                                , ( ( 3, 5 ), Tileset.arrow_right )
                                                , ( ( 4, 5 ), Tileset.arrow_right )
                                                , ( ( 5, 5 ), Tileset.letter_exclamation_mark )
                                                , ( ( 6, 5 ), Tileset.arrow_right )
                                                , ( ( 7, 5 ), Tileset.arrow_right )
                                                , ( ( 8, 5 ), Tileset.arrow_right )
                                                , ( ( 9, 5 ), Tileset.letter_exclamation_mark )
                                                , ( ( 10, 5 ), Tileset.arrow_right )
                                                ]

                                            3 ->
                                                [ ( ( 2, 5 ), Tileset.arrow_right )
                                                , ( ( 3, 5 ), Tileset.arrow_right )
                                                , ( ( 4, 5 ), Tileset.arrow_right )
                                                , ( ( 5, 5 ), Tileset.arrow_right )
                                                , ( ( 6, 5 ), Tileset.arrow_right )
                                                , ( ( 7, 5 ), Tileset.arrow_right )
                                                , ( ( 8, 5 ), Tileset.arrow_down )
                                                , ( ( 9, 5 ), Tileset.arrow_down )
                                                , ( ( 10, 5 ), Tileset.arrow_right )
                                                , ( ( 11, 5 ), Tileset.letter_exclamation_mark )
                                                ]

                                            2 ->
                                                [ ( ( 2, 5 ), Tileset.arrow_down )
                                                , ( ( 3, 5 ), Tileset.arrow_right )
                                                , ( ( 4, 5 ), Tileset.arrow_right )
                                                , ( ( 5, 5 ), Tileset.arrow_right )
                                                , ( ( 6, 5 ), Tileset.arrow_right )
                                                , ( ( 7, 5 ), Tileset.letter_exclamation_mark )
                                                ]

                                            _ ->
                                                [ ( ( 2, 5 ), Tileset.arrow_down )
                                                , ( ( 3, 5 ), Tileset.arrow_right )
                                                , ( ( 4, 5 ), Tileset.arrow_right )
                                                , ( ( 5, 5 ), Tileset.arrow_right )
                                                , ( ( 6, 5 ), Tileset.letter_exclamation_mark )
                                                ]
                                        )
                                )
                    in
                    case oldScreen of
                        Just justOldScreen ->
                            Transition.customTransition
                                "next_level"
                                [ ( 0, "overflow:hidden;width:" ++ (toString <| scale * tileset.spriteWidth * width) ++ "px;" )
                                , ( 2, "overflow:hidden;width:0px;" )
                                ]
                                |> Transition.apply
                                    options
                                    { from = justOldScreen
                                    , to = tutorialWorldScreen
                                    }

                        Nothing ->
                            if player.lifes > 0 then
                                Graphics.render options tutorialWorldScreen
                            else
                                Transition.customTransition
                                    "death_transition"
                                    [ ( 0, "opacity:1;filter:grayscale(0%) blur(0px);" )
                                    , ( 1, "opacity:1;filter:grayscale(70%) blur(0px);" )
                                    , ( 3, "opacity:0;filter:grayscale(70%) blur(5px);" )
                                    ]
                                    |> Transition.apply
                                        options
                                        { from = tutorialWorldScreen
                                        , to = deathScreen
                                        }

        Nothing ->
            Graphics.render options menuScreen


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
