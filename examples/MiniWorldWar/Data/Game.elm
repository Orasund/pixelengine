module MiniWorldWar.Data.Game exposing
    ( Game
    , GameState(..)
    , addMoveBoard
    , decoder
    , encode
    , new
    , nextRound
    )

import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
import MiniWorldWar.Data.Board as Board exposing (Move, MoveBoard, Supply(..), SupplyBoard, Unit, UnitBoard)
import MiniWorldWar.Data.Color as Color exposing (Color(..))
import MiniWorldWar.Data.Continent as Continent exposing (Continent(..), list)
import MiniWorldWar.Data.Direction exposing (Direction(..))
import Random exposing (Generator)
import Time exposing (Posix)


type GameState
    = Win Color
    | Draw
    | HostReady
    | BothReady
    | Running


type alias Game =
    { lastUpdated : Int
    , unitBoard : UnitBoard
    , moveBoard : MoveBoard
    , supplyBoard : SupplyBoard
    , state : GameState
    }


updateUnit : Unit -> Maybe Unit -> Maybe Unit
updateUnit { color, amount } =
    \maybeUnit ->
        case maybeUnit of
            Just unit ->
                if color == unit.color then
                    Just
                        { color = color
                        , amount = unit.amount + amount
                        }

                else if unit.amount == amount then
                    Nothing

                else if unit.amount > amount then
                    Just
                        { color = unit.color
                        , amount = unit.amount - amount
                        }

                else
                    Just
                        { color = color
                        , amount = amount - unit.amount
                        }

            Nothing ->
                Just { color = color, amount = amount }


applyMove : Continent -> Maybe Move -> Color -> UnitBoard -> UnitBoard
applyMove continent maybeMove color =
    case maybeMove of
        Just { direction, amount } ->
            let
                targetContinent =
                    continent |> Continent.neighbour direction
            in
            Board.update continent
                (updateUnit
                    { color = color |> Color.flip
                    , amount = amount
                    }
                )
                >> Board.update targetContinent
                    (updateUnit { color = color, amount = amount })

        Nothing ->
            identity


addMoveBoard : MoveBoard -> Game -> Game
addMoveBoard board game =
    { game
        | moveBoard = game.moveBoard |> Board.combine board
    }


applyNewUnits : SupplyBoard -> UnitBoard -> UnitBoard
applyNewUnits newUnitBoard unitBoard =
    let
        addUnit : Int -> Maybe Unit -> Maybe Unit
        addUnit n maybeUnit =
            maybeUnit
                |> Maybe.map
                    (\({ amount } as unit) ->
                        { unit | amount = amount + n }
                    )
    in
    Continent.list
        |> List.foldl
            (\continent ->
                Board.update
                    continent
                    (addUnit
                        (newUnitBoard
                            |> Board.get continent
                            |> Board.maybeSupplyToInt
                        )
                    )
            )
            unitBoard


nextRound : Posix -> Game -> Generator Game
nextRound time ({ moveBoard } as game) =
    let
        maybeSupplyGenerator : Generator (Maybe Supply)
        maybeSupplyGenerator =
            Random.weighted ( 3, Nothing ) [ ( 3, Just Supply ) ]

        supplyBoardGenerator : Generator SupplyBoard
        supplyBoardGenerator =
            Random.map5
                (\africa asia southAmerica europe northAmerica ->
                    { europe = europe
                    , northAmerica = northAmerica
                    , southAmerica = southAmerica
                    , asia = asia
                    , africa = africa
                    }
                )
                maybeSupplyGenerator
                maybeSupplyGenerator
                maybeSupplyGenerator
                maybeSupplyGenerator
                maybeSupplyGenerator

        state : UnitBoard -> GameState
        state unitBoard =
            case
                Continent.list
                    |> List.filterMap
                        (\continent -> unitBoard |> Board.get continent)
                    |> List.partition (.color >> (==) Blue)
            of
                ( _ :: _, _ :: _ ) ->
                    Running

                ( [], _ :: _ ) ->
                    Win Red

                ( _ :: _, [] ) ->
                    Win Blue

                ( [], [] ) ->
                    Draw
    in
    Random.map
        (\supplyBoard ->
            let
                unitBoard : UnitBoard
                unitBoard =
                    Continent.list
                        |> List.foldl
                            (\continent ->
                                applyMove continent
                                    (moveBoard |> Board.get continent)
                                    (game.unitBoard
                                        |> Board.get continent
                                        |> Maybe.map .color
                                        |> Maybe.withDefault Blue
                                    )
                            )
                            game.unitBoard
                        |> applyNewUnits supplyBoard
            in
            { lastUpdated = time |> Time.posixToMillis
            , unitBoard = unitBoard
            , moveBoard = Board.empty
            , supplyBoard = supplyBoard
            , state = state unitBoard
            }
        )
        supplyBoardGenerator


new : Int -> Game
new time =
    { lastUpdated = time
    , moveBoard = Board.empty
    , unitBoard =
        { europe =
            Just
                { color = Red
                , amount = 1
                }
        , africa =
            Just
                { color = Red
                , amount = 1
                }
        , asia = Nothing
        , northAmerica =
            Just
                { color = Blue
                , amount = 1
                }
        , southAmerica =
            Just
                { color = Blue
                , amount = 1
                }
        }
    , supplyBoard = Board.empty
    , state = Running
    }



{------------------------
   Decoder
------------------------}


gameStateDecoder : Decoder GameState
gameStateDecoder =
    D.map
        (\state ->
            case state of
                "Running" ->
                    Running

                "HostReady" ->
                    HostReady

                "BothReady" ->
                    BothReady

                "BlueWins" ->
                    Win Blue

                "RedWins" ->
                    Win Red

                "Draw" ->
                    Draw

                _ ->
                    Running
        )
        D.string


decoder : Decoder Game
decoder =
    D.map5
        (\lastUpdated moveBoard unitBoard supplyBoard state ->
            { lastUpdated = lastUpdated
            , unitBoard = unitBoard
            , moveBoard = moveBoard
            , supplyBoard = supplyBoard
            , state = state
            }
        )
        (D.field "lastUpdated" D.int)
        (D.field "moveBoard" Board.moveBoardDecoder)
        (D.field "unitBoard" Board.unitBoardDecoder)
        (D.field "supplyBoard" Board.supplyBoardDecoder)
        (D.field "state" gameStateDecoder)



{------------------------
   Encoder
------------------------}


gameStateEncoder : GameState -> Value
gameStateEncoder state =
    case state of
        Running ->
            E.string "Running"

        HostReady ->
            E.string "HostReady"

        BothReady ->
            E.string "BothReady"

        Win Blue ->
            E.string "BlueWins"

        Win Red ->
            E.string "RedWins"

        Draw ->
            E.string "Draw"


encode : Game -> Value
encode { lastUpdated, moveBoard, unitBoard, supplyBoard, state } =
    E.object
        [ ( "lastUpdated", E.int lastUpdated )
        , ( "moveBoard", Board.moveBoardEncoder moveBoard )
        , ( "unitBoard", Board.unitBoardEncoder unitBoard )
        , ( "supplyBoard", Board.supplyBoardEncoder supplyBoard )
        , ( "state", gameStateEncoder state )
        ]
