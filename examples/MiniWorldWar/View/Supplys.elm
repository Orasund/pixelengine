module MiniWorldWar.View.Supplys exposing (view)

import MiniWorldWar.Data.Board as Board exposing (Supply, SupplyBoard, UnitBoard)
import MiniWorldWar.Data.Continent exposing (Continent(..))
import MiniWorldWar.View as View
import MiniWorldWar.View.Image.SelectGui as Gui
import PixelEngine.Image exposing (Image)


drawSupply : Continent -> ( Float, Float ) -> Maybe Supply -> Maybe ( ( Float, Float ), Image msg )
drawSupply continent ( x1, y1 ) maybeSupply =
    maybeSupply
        |> Maybe.map
            (\_ ->
                let
                    ( x, y ) =
                        continent |> View.continentToPosition
                in
                ( ( x + x1, y + y1 )
                , Gui.supply
                )
            )


drawSupplys : ( Float, Float ) -> UnitBoard -> SupplyBoard -> List Continent -> List ( ( Float, Float ), Image msg )
drawSupplys pos unitBoard supplyBoard =
    List.filterMap
        (\continent ->
            unitBoard
                |> Board.get continent
                |> Maybe.andThen
                    (always
                        (supplyBoard
                            |> Board.get continent
                            |> drawSupply continent
                                pos
                        )
                    )
        )


view : UnitBoard -> SupplyBoard -> List ( List Continent, ( Float, Float ) ) -> List ( ( Float, Float ), Image msg )
view unitBoard supplyBoard list =
    list
        |> List.concatMap
            (\( continents, offset ) ->
                continents
                    |> drawSupplys
                        offset
                        unitBoard
                        supplyBoard
            )
