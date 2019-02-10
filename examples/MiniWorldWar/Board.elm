module MiniWorldWar.Board exposing (Board,Unit,decoder,get,set,update)

import MiniWorldWar.Color as Color exposing (Color)
import Json.Decode as D exposing (Decoder)
import MiniWorldWar.Continent as Continent exposing (Continent(..))

type alias Unit =
    { color : Color
    , amount : Int
    }

type alias Board =
    { europe : Maybe Unit
    , africa : Maybe Unit
    , asia : Maybe Unit
    , northAmerica : Maybe Unit
    , southAmerica : Maybe Unit
    }

get : Continent -> Board -> Maybe Unit
get continent =
    case continent of
        Europe -> .europe
        Africa -> .africa
        Asia -> .asia
        NorthAmerica -> .northAmerica
        SouthAmerica -> .southAmerica

set : Continent -> Maybe Unit -> Board -> Board
set continent value board=
    case continent of
        Europe -> {board| europe = value}
        Africa -> {board| africa = value}
        Asia -> {board| asia = value}
        NorthAmerica -> {board| northAmerica = value}
        SouthAmerica -> {board| southAmerica = value}

update : Continent -> (Maybe Unit -> Maybe Unit) -> Board -> Board
update continent fun board =
    board |> set continent (fun  (board |> get continent))
    

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

decoder : Decoder Board
decoder =
    D.map5
        (\europe africa asia northAmerica southAmerica ->
            { europe = europe
            , africa = africa
            , asia = asia
            , northAmerica = northAmerica
            , southAmerica = southAmerica
            }
        )
        (D.field "europe" unitDecoder)
        (D.field "africa" unitDecoder)
        (D.field "asia" unitDecoder)
        (D.field "northAmerica" unitDecoder)
        (D.field "southAmerica" unitDecoder)