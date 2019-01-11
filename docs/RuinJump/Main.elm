module RuinJump.Main exposing (main)

import Css
import Dict exposing (Dict)
import List.Zipper as Zipper exposing (Zipper)
import PixelEngine exposing (PixelEngine, program)
import PixelEngine.Controls exposing (Input(..))
import PixelEngine.Graphics as Graphics exposing (Area, Options)
import PixelEngine.Graphics.Tile exposing (Tile, Tileset)
import Process
import Random exposing (Generator)
import RuineJump.Config as Config
import RuineJump.Map as Map exposing (Map)
import RuineJump.MapElement as MapElement exposing (Block(..), MapElement(..))
import RuineJump.MapSegment as MapSegment
import RuineJump.MapSlice as MapSlice
import RuineJump.Player as Player exposing (FaceingDirection(..), Player, PlayerAction(..))
import Task
import Time


type alias Model =
    { map : Map
    , lowestY : Int
    , currentY : Int
    , xSlice : Zipper Int
    , player : Player
    , decaySpeed : Int
    , seed : Random.Seed
    }


type Msg
    = Init Int
    | Tick
    | Input Input


tickTask : Cmd Msg
tickTask =
    let
        delay =
            200
    in
    Task.perform (always Tick)
        (Process.sleep delay
            |> Task.andThen (\_ -> Time.now)
        )


restart : ( Maybe Model, Cmd Msg )
restart =
    ( Nothing
    , Random.generate Init <| Random.int Random.minInt Random.maxInt
    )


init : Int -> ( Maybe Model, Cmd Msg )
init int =
    let
        ( { map, xSlice }, seed ) =
            Random.initialSeed int
                |> Random.step
                    (MapSegment.concat
                        (List.range 0 10
                            |> List.map
                                (\i ->
                                    [ MapSegment.parkourGenerator <| i * 5 + 1
                                    , MapSegment.parkourGenerator <| i * 5 + 2
                                    , MapSegment.parkourGenerator <| i * 5 + 3
                                    , MapSegment.parkourGenerator <| i * 5 + 4
                                    , MapSegment.intersectionGenerator <| i * 5 + 5
                                    ]
                                )
                            |> List.concat
                            |> List.append [ MapSegment.floorGenerator 0 ]
                        )
                        |> Random.andThen
                            (\newMap ->
                                (newMap |> MapSlice.generator lowestY)
                                    |> Random.map
                                        (\newXSlice ->
                                            { map = newMap
                                            , xSlice = newXSlice
                                            }
                                        )
                            )
                    )

        (( _, y ) as pos) =
            ( Config.width // 2, -7 )

        lowestY : Int
        lowestY =
            0

        decaySpeed : Int
        decaySpeed =
            1

        player : Player
        player =
            { pos = pos, action = Standing, faceing = FaceingLeft }
    in
    ( Just
        { seed = seed
        , player = player
        , map = map
        , lowestY = lowestY
        , currentY = y
        , xSlice = xSlice
        , decaySpeed = decaySpeed
        }
    , tickTask
    )


removeOne : { a | xSlice : Zipper Int, lowestY : Int, map : Map } -> Random.Generator { a | xSlice : Zipper Int, lowestY : Int, map : Map }
removeOne ({ xSlice, lowestY, map } as rec) =
    let
        x : Int
        x =
            xSlice |> Zipper.current
    in
    case xSlice |> Zipper.next of
        Just slice ->
            Random.constant
                { rec
                    | xSlice = slice
                    , map = map |> Map.remove ( x, lowestY )
                }

        Nothing ->
            MapSlice.generator (lowestY - 1) map
                |> Random.map
                    (\slice ->
                        { rec
                            | xSlice = slice
                            , lowestY = lowestY - 1
                            , map = map |> Map.remove ( x, lowestY )
                        }
                    )


removeN : Int -> Model -> Model
removeN decaySpeed ({ seed } as model) =
    let
       ( newModel, newSeed ) =
           List.range 1 decaySpeed
               |> List.foldl
                   (always
                       (\( m, s ) ->
                           s
                               |> Random.step
                                   (m |> removeOne)
                       )
                   )
                   ( model, seed )
   in
   { newModel
       | seed = newSeed
   }


applyAction : (Player -> Player) -> Model -> Model
applyAction action ({ player, map, lowestY, xSlice, seed, decaySpeed } as model) =
    { model
        | player = player |> action
    }

placeBlock : Model -> Model
placeBlock ({ map, seed, decaySpeed, player } as model) =
    let
        ( x, y ) =
            player.pos

        (pos1,pos2) =
            case player.faceing of
                FaceingLeft ->
                    (( x - 2, y )
                    ,( x - 1, y + 1)
                    )

                FaceingRight ->
                    (( x + 3, y )
                    ,( x + 2, y + 1 )
                    )

        ( elem, newSeed ) =
            Random.step MapElement.woodGenerator seed
    in
    { model
        | map = map |> Dict.insert pos2 elem |> Dict.insert pos1 elem
        , decaySpeed = decaySpeed + 1
        , seed = newSeed
    }


onInput : Input -> Model -> Maybe ( Model, Cmd Msg )
onInput input ({ map,decaySpeed } as model) =
    (case input of
        InputUp ->
            Just <| (removeN decaySpeed << (applyAction <| Player.jump map))

        InputLeft ->
            Just <| applyAction <| Player.move FaceingLeft map

        InputDown ->
            Just <| applyAction <| Player.drop map

        InputRight ->
            Just <| applyAction <| Player.move FaceingRight map

        InputA ->
            Just <| placeBlock

        _ ->
            Nothing
    )
        |> Maybe.map
            (\function ->
                model
                    |> function
                    |> (\newModel ->
                            ( newModel
                            , tickTask
                            )
                       )
            )


onTick : Model -> ( Maybe Model, Cmd Msg )
onTick ({ map, player, lowestY } as model) =
    let
        ( _, playerY ) =
            player.pos
    in
    if lowestY <= playerY then
        restart

    else
        let
            { newPlayer, nextTick } =
                Player.update
                    player
                    map
                    (\elem ->
                        case elem of
                            Nothing ->
                                False

                            Just (BlockElement Air _) ->
                                False

                            Just _ ->
                                True
                    )

            ( _, y ) =
                newPlayer.pos
        in
        if nextTick then
            ( Just { model | player = newPlayer }
            , tickTask
            )

        else
            ( Just
                { model
                    | player = newPlayer
                    , currentY = y
                }
            , Cmd.none
            )


update : Msg -> Maybe Model -> ( Maybe Model, Cmd Msg )
update msg maybeModel =
    let
        defaultCase =
            ( maybeModel, Cmd.none )
    in
    case maybeModel of
        Nothing ->
            case msg of
                Init int ->
                    init int

                _ ->
                    defaultCase

        Just model ->
            case msg of
                Init int ->
                    init int

                Tick ->
                    model |> onTick

                Input input ->
                    case model |> onInput input of
                        Nothing ->
                            defaultCase

                        Just ( newModel, cmd ) ->
                            ( Just newModel, cmd )


subscriptions : Maybe Model -> Sub Msg
subscriptions _ =
    Sub.none


getTilesList : { currentY : Int, lowestY : Int } -> Map -> List ( ( Int, Int ), Tile Msg )
getTilesList { currentY, lowestY } =
    Dict.foldl
        (\pos elem list ->
            let
                ( posX, posY ) =
                    pos

                ( _, centerY ) =
                    ( (Config.width // 2) - 1
                    , (Config.width // 2) - 1
                    )

                lowestYModSection =
                    (lowestY // Config.sectionHeight)
                        |> (*) Config.sectionHeight

                heightModSection =
                    ((currentY + centerY + 2) // (Config.width // 2))
                        - 1
                        |> (*) (Config.width // 2)

                ( x, y ) =
                    ( posX
                    , if currentY > -1 * centerY - 1 + lowestYModSection then
                        posY
                            + Config.width
                            - 1
                            - lowestYModSection

                      else
                        posY
                            + Config.width
                            - 1
                            - heightModSection
                    )
            in
            list
                |> (if
                        (x >= 0)
                            && (x < Config.width)
                            && (y >= 0)
                            && (y < Config.width)
                    then
                        List.append <|
                            MapElement.toTiles
                                ( x, y )
                                elem

                    else
                        identity
                   )
        )
        []


view : Maybe Model -> { title : String, options : Options Msg, body : List (Area Msg) }
view maybeModel =
    let
        width : Float
        width =
            toFloat <| 3 * Config.width

        options =
            Graphics.options
                { width = width
                , transitionSpeedInSec = 0.5
                }

        rows : Int
        rows =
            Config.width

        tileset : Tileset
        tileset =
            { source = "tileset.png"
            , spriteWidth = 3
            , spriteHeight = 3
            }
    in
    { title = "Ruine Jump"
    , options = options
    , body =
        [ Graphics.tiledArea
            { background = Graphics.colorBackground <| Css.rgb 68 36 52
            , rows = rows
            , tileset = tileset
            }
            (case maybeModel of
                Just { map, player, currentY, lowestY } ->
                    map
                        |> Dict.insert
                            player.pos
                            (PlayerElement player.action player.faceing)
                        |> getTilesList
                            { currentY = currentY, lowestY = lowestY }

                Nothing ->
                    []
            )
        ]
    }


main : PixelEngine {} (Maybe Model) Msg
main =
    program
        { init = always restart
        , view = view
        , update = update
        , subscriptions = subscriptions
        , controls = Input
        }
