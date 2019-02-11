module MiniWorldWar.Data.Board exposing
    ( Board
    , Move
    , MoveBoard
    , Supply(..)
    , SupplyBoard
    , Unit
    , UnitBoard
    , combine
    , decoder
    , empty
    , get
    , maybeSupplyToInt
    , moveBoardDecoder
    , moveBoardEncoder
    , set
    , supplyBoardDecoder
    , supplyDecoder
    , unitBoardDecoder
    , unitBoardEncoder
    , supplyBoardEncoder
    , update
    )

import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
import MiniWorldWar.Data.Color as Color exposing (Color(..))
import MiniWorldWar.Data.Continent as Continent exposing (Continent(..))
import MiniWorldWar.Data.Direction as Direction exposing (Direction(..))


type alias Unit =
    { color : Color
    , amount : Int
    }


type alias Move =
    { amount : Int
    , direction : Direction
    }


type Supply
    = Supply


maybeSupplyToInt : Maybe Supply -> Int
maybeSupplyToInt newUnit =
    case newUnit of
        Nothing ->
            0

        Just Supply ->
            1


type alias Board a =
    { europe : Maybe a
    , africa : Maybe a
    , asia : Maybe a
    , northAmerica : Maybe a
    , southAmerica : Maybe a
    }


empty : Board a
empty =
    { europe = Nothing
    , africa = Nothing
    , asia = Nothing
    , northAmerica = Nothing
    , southAmerica = Nothing
    }


type alias UnitBoard =
    Board Unit


type alias MoveBoard =
    Board Move


type alias SupplyBoard =
    Board Supply


get : Continent -> Board a -> Maybe a
get continent =
    case continent of
        Europe ->
            .europe

        Africa ->
            .africa

        Asia ->
            .asia

        NorthAmerica ->
            .northAmerica

        SouthAmerica ->
            .southAmerica


set : Continent -> Maybe a -> Board a -> Board a
set continent value board =
    case continent of
        Europe ->
            { board | europe = value }

        Africa ->
            { board | africa = value }

        Asia ->
            { board | asia = value }

        NorthAmerica ->
            { board | northAmerica = value }

        SouthAmerica ->
            { board | southAmerica = value }


combine : Board a -> Board a -> Board a
combine boardA boardB =
    Continent.list
        |> List.foldl
            (\continent boardC ->
                boardC
                    |> (case ( boardA |> get continent, boardB |> get continent ) of
                            ( Just a, Nothing ) ->
                                set continent (Just a)

                            ( Nothing, Just b ) ->
                                set continent (Just b)

                            ( Just _, Just _ ) ->
                                identity

                            ( Nothing, Nothing ) ->
                                identity
                       )
            )
            empty


update : Continent -> (Maybe a -> Maybe a) -> Board a -> Board a
update continent fun board =
    board |> set continent (fun (board |> get continent))



{------------------------
   Decoder
------------------------}


supplyDecoder : Decoder (Maybe Supply)
supplyDecoder =
    D.map
        (\supply ->
            case supply of
                1 ->
                    Just Supply

                _ ->
                    Nothing
        )
        D.int


unitDecoder : Decoder (Maybe Unit)
unitDecoder =
    D.map2
        (\bool amount ->
            if amount == 0 then
                Nothing

            else
                Just
                    { color = bool |> Color.fromBool
                    , amount = amount
                    }
        )
        (D.field "isBlue" D.bool)
        (D.field "amount" D.int)


moveDecoder : Decoder (Maybe Move)
moveDecoder =
    D.map2
        (\string amount ->
            if amount == 0 then
                Nothing

            else
                Just
                    { direction = string |> Direction.fromString
                    , amount = amount
                    }
        )
        (D.field "direction" D.string)
        (D.field "amount" D.int)


decoder : Decoder (Maybe a) -> Decoder (Board a)
decoder a =
    D.map5
        (\europe africa asia northAmerica southAmerica ->
            { europe = europe
            , africa = africa
            , asia = asia
            , northAmerica = northAmerica
            , southAmerica = southAmerica
            }
        )
        (D.field "europe" a)
        (D.field "africa" a)
        (D.field "asia" a)
        (D.field "northAmerica" a)
        (D.field "southAmerica" a)


unitBoardDecoder : Decoder UnitBoard
unitBoardDecoder =
    decoder unitDecoder


supplyBoardDecoder : Decoder SupplyBoard
supplyBoardDecoder =
    decoder supplyDecoder


moveBoardDecoder : Decoder MoveBoard
moveBoardDecoder =
    decoder moveDecoder



{------------------------
   Encoder
------------------------}
supplyEncoder : Maybe Supply -> Value
supplyEncoder =
    maybeSupplyToInt
    >> E.int


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


moveEncoder : Maybe Move -> Value
moveEncoder maybeMove =
    let
        { direction, amount } =
            maybeMove
                |> Maybe.withDefault { direction = Up, amount = 0 }
    in
    E.object
        [ ( "direction", E.string (Direction.toString direction) )
        , ( "amount", E.int amount )
        ]


encoder : (Maybe a -> Value) -> Board a -> Value
encoder fun { europe, africa, asia, northAmerica, southAmerica } =
    E.object
        [ ( "europe", fun europe )
        , ( "africa", fun africa )
        , ( "asia", fun asia )
        , ( "northAmerica", fun northAmerica )
        , ( "southAmerica", fun southAmerica )
        ]


unitBoardEncoder : UnitBoard -> Value
unitBoardEncoder =
    encoder unitEncoder


moveBoardEncoder : MoveBoard -> Value
moveBoardEncoder =
    encoder moveEncoder

supplyBoardEncoder : SupplyBoard -> Value
supplyBoardEncoder =
    encoder supplyEncoder