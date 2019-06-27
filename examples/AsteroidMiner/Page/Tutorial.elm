module AsteroidMiner.Page.Tutorial exposing (Model, Msg, init, subscriptions, update, view)

import Action exposing (Action)
import AsteroidMiner.Building exposing (BuildingType(..), Code(..), Volume(..))
import AsteroidMiner.Data exposing (size, spriteSize)
import AsteroidMiner.Data.Game as Game exposing (Game)
import AsteroidMiner.Data.Item as Item exposing (Item(..))
import AsteroidMiner.Data.Map as Map exposing (Map)
import AsteroidMiner.Lib.Map exposing (SquareType(..))
import AsteroidMiner.View.RunningGame as RunningGame exposing (Status(..))
import AsteroidMiner.View.Tileset as Tileset
import Color
import Dict
import Grid.Bordered as Grid
import Grid.Position exposing (Position)
import PixelEngine exposing (Area)
import PixelEngine.Image as Image
import PixelEngine.Options as Options exposing (Options, Transition)
import PixelEngine.Tile as Tile exposing (Tile)
import Random exposing (Seed)


type alias Model =
    { num : Int
    , content : RunningGame.Model
    }


type Msg
    = Next
    | Exit
    | GameSpecific RunningGame.Msg


type alias TutorialAction =
    Action Model Never Never ()


maxTutorial : Int
maxTutorial =
    4


tutorial : Int -> Map -> Map
tutorial num map =
    (case num of
        1 ->
            [ ( ( 20, 20 ), Mine |> Game.newBuilding (Just Stone) )
            , ( ( 22, 20 ), Container Empty |> Game.newBuilding Nothing )

            --
            , ( ( 21, 18 ), ConveyorBelt Invalid |> Game.newBuilding Nothing )
            , ( ( 22, 18 ), Container Empty |> Game.newBuilding Nothing )

            --
            , ( ( 21, 16 ), ConveyorBelt Invalid |> Game.newBuilding Nothing )
            , ( ( 20, 16 ), Mine |> Game.newBuilding (Just Stone) )

            --
            , ( ( 20, 14 ), Mine |> Game.newBuilding (Just Stone) )
            , ( ( 21, 14 ), ConveyorBelt Invalid |> Game.newBuilding Nothing )
            , ( ( 22, 14 ), Container Empty |> Game.newBuilding Nothing )
            ]

        2 ->
            [ ( ( 20, 16 ), Mine |> Game.newBuilding (Just Stone) )
            , ( ( 21, 16 ), ConveyorBelt Invalid |> Game.newBuilding Nothing )
            , ( ( 22, 16 ), ConveyorBelt Invalid |> Game.newBuilding Nothing )
            , ( ( 23, 16 ), Container Empty |> Game.newBuilding Nothing )

            --
            , ( ( 20, 15 ), Mine |> Game.newBuilding (Just Stone) )
            , ( ( 23, 15 ), Container Empty |> Game.newBuilding Nothing )

            --
            , ( ( 20, 14 ), Mine |> Game.newBuilding (Just Stone) )
            , ( ( 23, 14 ), Container Empty |> Game.newBuilding Nothing )

            --
            , ( ( 19, 13 ), Mine |> Game.newBuilding (Just Stone) )
            , ( ( 20, 13 ), ConveyorBelt Invalid |> Game.newBuilding Nothing )
            , ( ( 21, 13 ), ConveyorBelt Invalid |> Game.newBuilding Nothing )
            , ( ( 22, 13 ), ConveyorBelt Invalid |> Game.newBuilding Nothing )
            , ( ( 23, 13 ), Container Empty |> Game.newBuilding Nothing )

            --
            , ( ( 18, 12 ), Mine |> Game.newBuilding (Just Stone) )
            , ( ( 23, 12 ), Container Empty |> Game.newBuilding Nothing )
            ]

        3 ->
            [ ( ( 20, 14 ), Mine |> Game.newBuilding (Just Stone) )
            , ( ( 21, 14 ), ConveyorBelt Invalid |> Game.newBuilding Nothing )
            , ( ( 22, 14 ), Container Empty |> Game.newBuilding Nothing )
            , ( ( 23, 14 ), Merger |> Game.newBuilding Nothing )
            , ( ( 23, 12 ), Container Empty |> Game.newBuilding Nothing )

            --
            , ( ( 20, 16 ), Mine |> Game.newBuilding (Just Stone) )
            , ( ( 21, 16 ), ConveyorBelt Invalid |> Game.newBuilding Nothing )
            , ( ( 22, 16 ), Container Empty |> Game.newBuilding Nothing )
            , ( ( 23, 16 ), Merger |> Game.newBuilding Nothing )
            , ( ( 23, 17 ), ConveyorBelt Invalid |> Game.newBuilding Nothing )
            , ( ( 23, 18 ), Container Empty |> Game.newBuilding Nothing )

            --
            , ( ( 19, 20 ), Mine |> Game.newBuilding (Just Stone) )
            , ( ( 20, 20 ), ConveyorBelt Invalid |> Game.newBuilding Nothing )
            , ( ( 21, 20 ), Container Empty |> Game.newBuilding Nothing )
            , ( ( 22, 20 ), Merger |> Game.newBuilding Nothing )
            , ( ( 22, 19 ), ConveyorBelt Invalid |> Game.newBuilding Nothing )
            , ( ( 22, 18 ), ConveyorBelt Invalid |> Game.newBuilding Nothing )

            --
            , ( ( 18, 12 ), Mine |> Game.newBuilding (Just Stone) )
            , ( ( 19, 12 ), ConveyorBelt Invalid |> Game.newBuilding Nothing )
            , ( ( 20, 12 ), Container Empty |> Game.newBuilding Nothing )
            , ( ( 22, 12 ), ConveyorBelt Invalid |> Game.newBuilding Nothing )
            ]

        4 ->
            [ ( ( 18, 12 ), Mine |> Game.newBuilding (Just Stone) )
            , ( ( 19, 13 ), Mine |> Game.newBuilding (Just Stone) )
            , ( ( 20, 12 ), Container Empty |> Game.newBuilding Nothing )

            --
            , ( ( 20, 15 ), Mine |> Game.newBuilding (Just Stone) )
            , ( ( 21, 15 ), ConveyorBelt Invalid |> Game.newBuilding Nothing )
            , ( ( 23, 15 ), Container Empty |> Game.newBuilding Nothing )
            , ( ( 22, 14 ), Container Empty |> Game.newBuilding Nothing )

            --
            , ( ( 20, 20 ), Mine |> Game.newBuilding (Just Stone) )
            , ( ( 19, 21 ), Mine |> Game.newBuilding (Just Stone) )
            , ( ( 20, 21 ), Sorter |> Game.newBuilding Nothing )
            , ( ( 21, 21 ), Container Empty |> Game.newBuilding Nothing )
            ]

        _ ->
            []
    )
        |> List.foldl
            (\( pos, building ) ->
                Grid.ignoringErrors
                    (Grid.update pos <| always <| Ok <| Just <| building)
            )
            map


init : Int -> Seed -> ( Model, Cmd msg )
init num seed =
    let
        content =
            RunningGame.init
                { map = Map.init |> tutorial num
                , seed = seed
                , winCondition =
                    case num of
                        1 ->
                            60

                        2 ->
                            80

                        3 ->
                            60

                        _ ->
                            60
                }
    in
    ( { num = num
      , content = content
      }
    , Cmd.none
    )


update : Msg -> Model -> TutorialAction
update msg ({ num } as model) =
    case msg of
        Next ->
            if num == maxTutorial then
                Action.exiting

            else
                Action.updating <|
                    init (num + 1) model.content.seed

        Exit ->
            Action.exiting

        GameSpecific gameMsg ->
            case model.content.status of
                Running ->
                    let
                        content : RunningGame.Model
                        content =
                            RunningGame.update gameMsg model.content
                    in
                    Action.updating ( { model | content = content }, Cmd.none )

                _ ->
                    Action.updating ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions { content } =
    content |> RunningGame.subscriptions |> Sub.map GameSpecific


areas : Model -> List (Area Msg)
areas { num, content } =
    case content.status of
        Won ->
            [ PixelEngine.imageArea
                { height = (toFloat <| size) * spriteSize
                , background =
                    PixelEngine.colorBackground <|
                        Color.rgb255 218 212 94
                }
                [ ( ( (toFloat <| (size // 2) - 4) * spriteSize, (toFloat <| size // 2) * spriteSize )
                  , Image.fromText "Game Won" Tileset.font
                        |> Image.clickable Next
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

        Running ->
            let
                text : String -> Position -> List ( Position, Tile msg )
                text t ( x, y ) =
                    Tile.fromText ( 0, 10 ) t
                        |> List.indexedMap
                            (\i letter ->
                                ( ( x + i, y ), letter )
                            )
            in
            content
                |> RunningGame.areas
                    (case num of
                        1 ->
                            List.concat
                                [ ( 15, 14 ) |> text "Mine>"
                                , ( 18, 11 ) |> text "Conveyor"
                                , ( 18, 12 ) |> text "Belt"
                                , ( 21, 13 ) |> text "v"
                                , ( 23, 13 ) |> text "container"
                                , ( 23, 14 ) |> text "<"

                                --
                                , ( 23, 16 ) |> text "<add"

                                --
                                , ( 16, 18 ) |> text "add>"

                                --
                                , ( 21, 21 ) |> text "^add"
                                ]

                        2 ->
                            List.concat
                                [ ( 5, 1 ) |> text "touching belts"
                                , ( 5, 2 ) |> text "have different colors"

                                --
                                , ( 19, 10 ) |> text "1.add"
                                , ( 19, 11 ) |> text "v"

                                --
                                , ( 14, 14 ) |> text "2.add>"
                                , ( 14, 15 ) |> text "3.wait"

                                --
                                , ( 24, 15 ) |> text "<4.add"
                                ]

                        3 ->
                            List.concat
                                [ ( 5, 1 ) |> text "Merger takes items"
                                , ( 5, 2 ) |> text "from containers and"
                                , ( 5, 3 ) |> text "puts them on a belt"

                                --
                                , ( 24, 16 ) |> text "<Merger"

                                --
                                , ( 16, 9 ) |> text "add"
                                , ( 16, 10 ) |> text "Merger"
                                , ( 21, 11 ) |> text "v"

                                --
                                , ( 24, 13 ) |> text "<add"
                                , ( 25, 14 ) |> text "Belt"
                                ]

                        4 ->
                            List.concat
                                [ ( 5, 1 ) |> text "Sorter takes items"
                                , ( 5, 2 ) |> text "from a Belt or a Mine and"
                                , ( 5, 3 ) |> text "puts them into containers"

                                --
                                , ( 19, 10 ) |> text "add"
                                , ( 19, 11 ) |> text "v"

                                --
                                , ( 22, 16 ) |> text "^add"
                                , ( 21, 22 ) |> text "^"
                                , ( 21, 23 ) |> text "Sorter"
                                ]

                        _ ->
                            Tile.fromText ( 0, 10 ) "test"
                                |> List.indexedMap
                                    (\i letter ->
                                        ( ( 0 + i, 0 ), letter )
                                    )
                    )
                |> List.map (PixelEngine.mapArea GameSpecific)


view :
    (Msg -> msg)
    -> Options msg
    -> Model
    -> { options : Options msg, body : List (Area msg) }
view mapper options ({ content } as model) =
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
            |> (case content.status of
                    Running ->
                        identity

                    _ ->
                        Options.withTransitionFrom
                            (content
                                |> RunningGame.areas []
                                |> List.map (PixelEngine.mapArea (GameSpecific >> mapper))
                            )
                            transition
               )
    , body = model |> areas |> List.map (PixelEngine.mapArea mapper)
    }
