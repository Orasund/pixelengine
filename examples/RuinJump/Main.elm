module RuinJump.Main exposing (main)

import Color
import Dict
import PixelEngine exposing (Area, Input(..), PixelEngine, game)
import PixelEngine.Options as Options exposing (Options)
import PixelEngine.Tile exposing (Tile, Tileset)
import Process
import Random exposing (Generator, Seed)
import RuinJump.Config as Config
import RuinJump.Map exposing (Map)
import RuinJump.MapElement as MapElement exposing (Block(..), MapElement(..))
import RuinJump.MapSegment as MapSegment
import RuinJump.Player as Player
    exposing
        ( FaceingDirection(..)
        , Player
        , PlayerAction(..)
        )
import RuinJump.Stage as Stage exposing (Stage)
import Task
import Time


type alias Model =
    { stage : Stage
    , player : Player
    }


type alias State =
    ( Model, Seed )


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


restart : ( Maybe State, Cmd Msg )
restart =
    ( Nothing
    , Random.generate Init <| Random.int Random.minInt Random.maxInt
    )


init : Int -> ( Maybe State, Cmd Msg )
init int =
    let
        segments : Int -> List (Generator Map)
        segments i =
            [ MapSegment.parkourGenerator <| i * 5 + 1
            , MapSegment.parkourGenerator <| i * 5 + 2
            , MapSegment.parkourGenerator <| i * 5 + 3
            , MapSegment.parkourGenerator <| i * 5 + 4
            , MapSegment.intersectionGenerator <| i * 5 + 5
            ]

        (( _, y ) as pos) =
            ( Config.width // 2, -7 )

        ( stage, seed ) =
            Random.initialSeed int
                |> Random.step
                    (Stage.generate
                        { lowestY = 0
                        , currentY = y
                        , decaySpeed = 1
                        }
                        segments
                    )

        player : Player
        player =
            { pos = pos, action = Standing, faceing = FaceingLeft }
    in
    ( Just
        ( { player = player
          , stage = stage
          }
        , seed
        )
    , tickTask
    )


applyAction : (Player -> Player) -> Model -> Model
applyAction action ({ player } as model) =
    { model
        | player = player |> action
    }


placeStairs : Model -> Generator Model
placeStairs ({ player, stage } as model) =
    let
        ( x, y ) =
            player.pos

        ( pos1, pos2 ) =
            case player.faceing of
                FaceingLeft ->
                    ( ( x - 2, y )
                    , ( x - 1, y + 1 )
                    )

                FaceingRight ->
                    ( ( x + 3, y )
                    , ( x + 2, y + 1 )
                    )
    in
    Stage.placeStairs pos1 pos2 stage
        |> Random.map
            (\newStage ->
                { model
                    | stage = newStage
                }
            )


onInput : Input -> State -> Maybe ( State, Cmd Msg )
onInput input (( model, _ ) as state) =
    let
        { map, decaySpeed } =
            model.stage
    in
    (case input of
        InputUp ->
            Just
                ((Player.jump map |> applyAction |> Tuple.mapFirst)
                    >> (\( { stage } as m, seed ) ->
                            seed
                                |> Random.step
                                    (Stage.removeN decaySpeed stage
                                        |> Random.map
                                            (\s -> { m | stage = s })
                                    )
                       )
                )

        InputLeft ->
            Just
                (Player.move FaceingLeft map |> applyAction |> Tuple.mapFirst)

        InputDown ->
            Just (Player.drop map |> applyAction |> Tuple.mapFirst)

        InputRight ->
            Just (Player.move FaceingRight map |> applyAction |> Tuple.mapFirst)

        InputA ->
            Just (\( m, s ) -> s |> Random.step (placeStairs m))

        _ ->
            Nothing
    )
        |> Maybe.map
            (\function ->
                state
                    |> function
                    |> (\newState ->
                            ( newState
                            , tickTask
                            )
                       )
            )


onTick : State -> ( Maybe State, Cmd Msg )
onTick ( model, seed ) =
    let
        { player, stage } =
            model

        { map, lowestY } =
            stage

        ( _, playerY ) =
            player.pos
    in
    if lowestY <= playerY then
        restart

    else
        let
            { newPlayer, nextTick } =
                Player.update player map MapElement.isOccupied

            ( _, y ) =
                newPlayer.pos
        in
        if nextTick then
            ( Just ( { model | player = newPlayer }, seed )
            , tickTask
            )

        else
            ( Just
                ( { model
                    | player = newPlayer
                    , stage =
                        { stage
                            | currentY = y
                        }
                  }
                , seed
                )
            , Cmd.none
            )


update : Msg -> Maybe State -> ( Maybe State, Cmd Msg )
update msg maybeState =
    let
        defaultCase =
            ( maybeState, Cmd.none )
    in
    case maybeState of
        Nothing ->
            case msg of
                Init int ->
                    init int

                _ ->
                    defaultCase

        Just state ->
            case msg of
                Init int ->
                    init int

                Tick ->
                    state |> onTick

                Input input ->
                    case state |> onInput input of
                        Nothing ->
                            defaultCase

                        Just ( newState, cmd ) ->
                            ( Just newState, cmd )


subscriptions : Maybe State -> Sub Msg
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


width : Float
width =
    toFloat <| 3 * Config.width


view : Maybe State -> { title : String, options : Maybe (Options Msg), body : List (Area Msg) }
view maybeState =
    let
        options =
            Options.default
                |> Options.withMovementSpeed 0.5

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
    , options = Just options
    , body =
        [ PixelEngine.tiledArea
            { background = PixelEngine.colorBackground <| Color.rgb255 68 36 52
            , rows = rows
            , tileset = tileset
            }
            (case maybeState of
                Just ( { player, stage }, _ ) ->
                    let
                        { map, currentY, lowestY } =
                            stage
                    in
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


main : PixelEngine {} (Maybe State) Msg
main =
    game
        { init = always restart
        , view = view
        , update = update
        , subscriptions = subscriptions
        , controls = Input >> Just
        , width = width
        }
