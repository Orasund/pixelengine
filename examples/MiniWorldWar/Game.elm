module MiniWorldWar.Game exposing
    ( Game
    , decoder
    , encode
    , new
    , empty
    , newRound
    )

import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
import MiniWorldWar.Color as Color exposing (Color(..))
import Random exposing (Generator, Seed)
import MiniWorldWar.Board as Board exposing (Board,Unit)
import MiniWorldWar.Continent exposing (Continent(..),list)
import Time exposing (Posix)

type alias Game =
    { lastUpdated : Int
    , board : Board
    }


decoder : Decoder Game
decoder =
    D.map2
        (\lastUpdated board ->
            { lastUpdated = lastUpdated
            , board = board
            }
        )
        (D.field "lastUpdated" D.int)
        (D.field "board" Board.decoder)

newRound : Posix -> Game -> Generator Game
newRound time {board}  =
    let
        unitGenerator =
            (Random.weighted (1,0) [(1,2),(4,1)])
        
        addUnit : Int -> Maybe Unit -> Maybe Unit
        addUnit n maybeUnit =
            maybeUnit
            |> Maybe.map
                (\({amount} as unit) ->
                    {unit|amount=amount+n}
                )
    in
    Random.map5
    (\europe africa asia northAmerica southAmerica ->
        { lastUpdated = time |> Time.posixToMillis
        , board =
            board
            |> Board.update Europe (addUnit (europe+1) )
            |> Board.update Africa (addUnit africa)
            |> Board.update Asia (addUnit asia)
            |> Board.update NorthAmerica (addUnit (northAmerica+1))
            |> Board.update SouthAmerica (addUnit southAmerica)
        }
    )
    unitGenerator
    unitGenerator
    unitGenerator
    unitGenerator
    unitGenerator

new : Posix ->  Generator Game
new time =
    Random.constant
        { lastUpdated = time |> Time.posixToMillis
        , board =
            { europe =
                Just
                    { color = Red
                    , amount = 0
                    }
            , africa = Nothing
            , asia = Nothing
            , northAmerica =
                Just
                    { color = Blue
                    , amount = 0
                    }
            , southAmerica = Nothing
            }
        }

empty : Int -> Game
empty time =
    { lastUpdated = time
    , board =
        { europe = Nothing
        , africa = Nothing
        , asia = Nothing
        , northAmerica = Nothing
        , southAmerica = Nothing
        }
    }


encode : Game -> Value
encode { lastUpdated, board } =
    let
        unitEncoder : Maybe Unit -> Value
        unitEncoder maybeUnit =
            let
                { color, amount } =
                    maybeUnit
                        |> Maybe.withDefault { color = Blue, amount = 0 }
            in
            E.object
                [ ( "isBlue", E.bool (Color.isBlue color) )
                , ( "amount", E.int amount )
                ]

        boardEncoder : Board -> Value
        boardEncoder { europe, africa, asia, northAmerica, southAmerica } =
            E.object
                [ ( "europe", unitEncoder europe )
                , ( "africa", unitEncoder africa )
                , ( "asia", unitEncoder asia )
                , ( "northAmerica", unitEncoder northAmerica )
                , ( "southAmerica", unitEncoder southAmerica )
                ]
    in
    E.object
        [ ( "lastUpdated", E.int lastUpdated )
        , ( "board", boardEncoder board )
        ]
