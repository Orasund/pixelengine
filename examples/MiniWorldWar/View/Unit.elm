module MiniWorldWar.View.Unit exposing
    ( drawCenter
    , draw
    )

import MiniWorldWar.Board as Board exposing (Unit)
import PixelEngine.Graphics.Image as Image exposing (Image, image)
import MiniWorldWar.Continent as Continent exposing (Continent(..))
import MiniWorldWar.Direction as Direction exposing (Direction(..))
import MiniWorldWar.Color as Color exposing (Color)
import MiniWorldWar.View as View exposing (tileSize,continentToPosition)
import MiniWorldWar.Unit as Unit


drawUnit : ( Float, Float ) -> Unit -> ( ( Float, Float ), Image msg )
drawUnit pos unit =
    ( pos
    , Unit.unitImage unit { used = True }
    )


drawCenter : Continent -> { used : Bool } -> (Continent -> msg) -> Color -> Unit -> ( ( Float, Float ), Image msg )
drawCenter continent ({ used } as config) toMsg color unit =
    let
        ( x, y ) =
            continent |> continentToPosition
    in
    
        ( ( x + tileSize / 2, y + tileSize * 1 )
        , Unit.unitImage unit config
            |> if color == unit.color then
                Image.withAttributes
                    [ Image.onClick (toMsg continent) ]
                else
                    identity
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

draw : Direction -> Continent -> Unit -> ( ( Float, Float ), Image msg )
draw direction =
  case direction of
    Up ->
      drawUnitUp
    Down ->
      drawUnitDown
    Left ->
      drawUnitLeft
    Right ->
      drawUnitRight