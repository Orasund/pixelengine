module Roguelike.Main exposing (main)

--import Roguelike.Inventory as Inventory exposing (Inventory)

import Char
import Css exposing (px)
import Dict
import Html.Styled exposing (Html, program)
import Keyboard
import PixelEngine.Graphics as Graphics exposing (tile)
import Random
import Roguelike.Cell as Cell
    exposing
        ( Cell(..)
        , ConsumableType(..)
        , EnemyType(..)
        , Item(..)
        , MiscellaneousType(..)
        , SolidType(..)
        )
import Roguelike.Game as Game
import Roguelike.Inventory as Inventory
import Roguelike.Map as Map exposing (Direction(..), Map)
import Roguelike.Player as Player exposing (PlayerData)
import SelectList exposing (SelectList)


type alias Model =
    { map : SelectList (Map Cell)
    , player : PlayerData
    , seed : Random.Seed
    , worldSeed : Int
    , worldSize : Int
    }


type Input
    = Direction Direction
    | Drop
    | Activate
    | RotateLeft
    | RotateRight


type Msg
    = Input Input
    | Idle


updateSelected : (a -> a) -> SelectList a -> SelectList a
updateSelected fun list =
    list
        |> SelectList.mapBy
            (\pos ->
                if pos == SelectList.Selected then
                    fun
                else
                    identity
            )


init : Int -> ( Model, Cmd Msg )
init worldSeed =
    let
        worldSize : Int
        worldSize =
            16

        backpackSize : Int
        backpackSize =
            8

        ( currentMap, currentSeed ) =
            Map.generate
                (worldSize - 1)
                Cell.mapGenerator
                (Random.initialSeed worldSeed)
                |> Tuple.mapFirst (Dict.update ( 7, 7 ) (always (Just (Player Down))))
                |> Tuple.mapFirst SelectList.singleton
    in
    { map = currentMap
    , seed = currentSeed
    , worldSeed = worldSeed
    , worldSize = worldSize
    , player = Player.init backpackSize
    }
        ! [ Cmd.none ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input input ->
            let
                ( playerData, map ) =
                    case input of
                        Drop ->
                            ( model.player, model.map |> SelectList.selected )
                                |> Player.drop

                        Activate ->
                            ( model.player, model.map |> SelectList.selected )
                                |> Player.activate

                        Direction dir ->
                            ( model.player, model.map |> SelectList.selected )
                                |> Game.applyDirection (model.worldSize - 1) dir

                        RotateLeft ->
                            ( model.player |> Player.rotateLeft, model.map |> SelectList.selected )

                        RotateRight ->
                            ( model.player |> Player.rotateRight, model.map |> SelectList.selected )
            in
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
                init (model.worldSeed + 7)
            else if playerData.lifes > 0 then
                { model
                    | player = playerData
                    , map = model.map |> updateSelected (always map)
                }
                    ! [ Cmd.none ]
            else
                init (model.worldSeed + 1)

        Idle ->
            model ! [ Cmd.none ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Keyboard.presses <|
        Char.fromCode
            >> (\char ->
                    case char of
                        'w' ->
                            Input (Direction Up)

                        's' ->
                            Input (Direction Down)

                        'd' ->
                            Input (Direction Right)

                        'a' ->
                            Input (Direction Left)

                        'f' ->
                            Input Drop

                        ' ' ->
                            Input Activate

                        'q' ->
                            Input RotateLeft

                        'e' ->
                            Input RotateRight

                        _ ->
                            Idle
               )


view : Model -> Html Msg
view model =
    let
        tileset : Graphics.Tileset
        tileset =
            { source = "tileset.png", height = 16, width = 16 }
        
        scale : Int
        scale = 2

        width : Int
        width = 16
    in
    Graphics.render { scale = scale, width = px <| toFloat <| scale * tileset.width * width }
        [ Graphics.tiledArea
            { height = 16
            , background = Graphics.Image "groundTile.png"
            , tileset = tileset
            }
            (model.map
                |> SelectList.selected
                |> Dict.foldl
                    (\pos cell list ->
                        ( pos
                        , Cell.getImage cell
                        )
                            :: list
                    )
                    []
            )
        , Graphics.tiledArea
            { height = 3
            , background = Graphics.Color (Css.rgb 20 12 28)
            , tileset = tileset
            }
            ([ ( ( 2, 0 ), tile ( 11, 13 ) ) --\/
             , ( ( 3, 0 ), tile ( 1, 13 ) ) --f
             , ( ( 4, 0 ), tile ( 10, 15 ) ) -- -
             , ( ( 5, 0 ), tile ( 0, 15 ) ) --d
             , ( ( 6, 0 ), tile ( 4, 13 ) ) --r
             , ( ( 7, 0 ), tile ( 3, 14 ) ) --o
             , ( ( 8, 0 ), tile ( 3, 15 ) ) --p
             , ( ( 2, 2 ), tile ( 11, 12 ) ) --/\
             , ( ( 3, 2 ), tile ( 4, 14 ) ) --s
             , ( ( 4, 2 ), tile ( 3, 15 ) ) -- p
             , ( ( 5, 2 ), tile ( 0, 12 ) ) --a
             , ( ( 6, 2 ), tile ( 0, 14 ) ) --c
             , ( ( 7, 2 ), tile ( 1, 12 ) ) --e
             , ( ( 8, 2 ), tile ( 10, 15 ) ) -- -
             , ( ( 9, 2 ), tile ( 5, 12 ) ) -- u
             , ( ( 10, 2 ), tile ( 4, 14 ) ) --s
             , ( ( 11, 2 ), tile ( 1, 12 ) ) --e

             --
             , ( ( 12, 0 ), tile ( 4, 12 ) ) --q
             , ( ( 13, 0 ), tile ( 11, 15 ) ) --<
             , ( ( 14, 0 ), tile ( 11, 14 ) ) -->
             , ( ( 15, 0 ), tile ( 1, 12 ) ) --e
             ]
                |> List.append
                    (case model.player.inventory |> Inventory.ground of
                        Just a ->
                            [ ( ( 0, 1 ), Cell.getImage (Item a) ) ]

                        Nothing ->
                            []
                    )
                |> List.append
                    (List.range 0 (model.player.lifes - 1)
                        |> List.map (\i -> ( ( 15 - i, 2 ), tile ( 4, 8 ) ))
                    )
                |> List.append
                    (model.player.inventory
                        |> Inventory.get
                        |> List.indexedMap
                            (\i a ->
                                ( ( 2 + i, 1 ), Cell.getImage (Item a) )
                            )
                    )
            )
        ]


main : Program Never Model Msg
main =
    program
        { init = init 31412
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
