module MiniWorldWar.View.Units exposing (view)

import MiniWorldWar.Data.Board exposing (Move, Unit)
import MiniWorldWar.Data.Color exposing (Color)
import MiniWorldWar.Data.Continent as Continent exposing (Continent(..))
import MiniWorldWar.Data.Direction exposing (Direction(..))
import MiniWorldWar.Data.Game exposing (GameState(..))
import MiniWorldWar.View exposing (continentToPosition, tileSize)
import MiniWorldWar.View.Image.Unit as Unit
import PixelEngine.Image as Image exposing (Image)


drawUnit : ( Float, Float ) -> Unit -> ( ( Float, Float ), Image msg )
drawUnit pos unit =
    ( pos
    , Unit.unitImage unit { used = True }
    )


drawCenter : Continent -> { used : Bool } -> (Continent -> msg) -> Color -> Unit -> ( ( Float, Float ), Image msg )
drawCenter continent config toMsg color unit =
    let
        ( x, y ) =
            continent |> continentToPosition
    in
    ( ( x + tileSize / 2, y + tileSize * 1 )
    , Unit.unitImage unit config
        |> (if color == unit.color && unit.amount > 1 then
                Image.clickable (toMsg continent)

            else
                identity
           )
    )


drawUnitLeft : Continent -> Unit -> ( ( Float, Float ), Image msg )
drawUnitLeft continent =
    let
        ( x, y ) =
            continent |> continentToPosition
    in
    drawUnit ( x - tileSize / 2, y + tileSize / 2 )


drawUnitRight : Continent -> Unit -> ( ( Float, Float ), Image msg )
drawUnitRight continent =
    let
        ( x, y ) =
            continent |> continentToPosition
    in
    drawUnit ( x + (3 * tileSize) / 2, y + (3 * tileSize) / 2 )


drawUnitUp : Continent -> Unit -> ( ( Float, Float ), Image msg )
drawUnitUp continent =
    let
        ( x, y ) =
            continent |> continentToPosition
    in
    drawUnit ( x, y - tileSize / 2 )


drawUnitDown : Continent -> Unit -> ( ( Float, Float ), Image msg )
drawUnitDown continent =
    let
        ( x, y ) =
            continent |> continentToPosition
    in
    drawUnit ( x + tileSize * 1, y + (5 * tileSize) / 2 )


drawDirectional : Direction -> Continent -> Unit -> ( ( Float, Float ), Image msg )
drawDirectional direction =
    case direction of
        Up ->
            drawUnitUp

        Down ->
            drawUnitDown

        Left ->
            drawUnitLeft

        Right ->
            drawUnitRight


view : Color -> { ready : Bool } -> GameState -> (Continent -> msg) -> (Continent -> Maybe Unit) -> (Continent -> Maybe Move) -> List ( ( Float, Float ), Image msg )
view playerColor { ready } state msg maybeUnit maybeMove =
    Continent.list
        |> List.map
            (\continent ->
                case continent |> maybeUnit of
                    Just ({ color } as unit) ->
                        case continent |> maybeMove of
                            Just ({ direction } as move) ->
                                let
                                    amount =
                                        unit.amount - move.amount
                                in
                                [ { amount = amount, color = color }
                                    |> drawCenter
                                        continent
                                        { used = True }
                                        msg
                                        playerColor
                                , { amount = move.amount, color = color }
                                    |> drawDirectional
                                        direction
                                        continent
                                ]

                            Nothing ->
                                [ unit
                                    |> drawCenter
                                        continent
                                        { used =
                                            unit.amount
                                                <= 1
                                                || (playerColor /= color)
                                                || ready
                                                || (state /= Running)
                                        }
                                        msg
                                        playerColor
                                ]

                    Nothing ->
                        []
            )
        |> List.concat
