module MiniWorldWar.View.SelectGui exposing (view)

import MiniWorldWar.Data.Continent exposing (Continent(..))
import MiniWorldWar.Data.Direction exposing (Direction(..))
import MiniWorldWar.View as View
import MiniWorldWar.View.Image.SelectGui as Gui
import PixelEngine.Image as Image exposing (Image)


relativeCoord : ( Int, Int ) -> ( Float, Float ) -> ( Float, Float )
relativeCoord ( x1, y1 ) ( x, y ) =
    ( x + 8 * (x1 |> toFloat), y + 3 + 8 * (y1 |> toFloat) )


addUnitButton : msg -> ( Float, Float ) -> List ( ( Float, Float ), Image msg )
addUnitButton msg pos =
    [ ( pos |> relativeCoord ( 3, 0 )
      , Gui.addUnitButton
            |> Image.clickable msg
      )
    ]


swapUnitsButton : msg -> ( Float, Float ) -> List ( ( Float, Float ), Image msg )
swapUnitsButton msg pos =
    [ ( pos |> relativeCoord ( 3, 1 )
      , Gui.swapUnitsButton
            |> Image.clickable msg
      )
    ]


removeUnitButton : msg -> ( Float, Float ) -> List ( ( Float, Float ), Image msg )
removeUnitButton msg pos =
    [ ( pos |> relativeCoord ( 3, 2 )
      , Gui.removeUnitButton
            |> Image.clickable msg
      )
    ]


cardButton : ( Int, Int ) -> msg -> ( Float, Float ) -> ( ( Float, Float ), Image msg )
cardButton relPos msg pos =
    ( pos |> relativeCoord relPos
    , Gui.cardButton
        |> Image.clickable msg
    )


centerCardButton : ( Int, Int ) -> msg -> ( Float, Float ) -> ( ( Float, Float ), Image msg )
centerCardButton relPos msg pos =
    ( pos |> relativeCoord relPos
    , Gui.centerCardButton
        |> Image.clickable msg
    )


locationButtons : Continent -> msg -> (Direction -> msg) -> ( Float, Float ) -> List ( ( Float, Float ), Image msg )
locationButtons continent centerMsg dirMsg pos =
    case continent of
        Asia ->
            [ pos |> centerCardButton ( 1, 1 ) centerMsg
            , pos |> cardButton ( 0, 1 ) (dirMsg Left)
            , pos |> cardButton ( 2, 1 ) (dirMsg Right)
            ]

        Africa ->
            [ pos |> centerCardButton ( 1, 2 ) centerMsg
            , pos |> cardButton ( 1, 1 ) (dirMsg Up)
            , pos |> cardButton ( 0, 2 ) (dirMsg Left)
            ]

        SouthAmerica ->
            [ pos |> centerCardButton ( 1, 2 ) centerMsg
            , pos |> cardButton ( 1, 1 ) (dirMsg Up)
            , pos |> cardButton ( 2, 2 ) (dirMsg Right)
            ]

        Europe ->
            [ pos |> centerCardButton ( 1, 1 ) centerMsg
            , pos |> cardButton ( 0, 1 ) (dirMsg Left)
            , pos |> cardButton ( 2, 1 ) (dirMsg Right)
            , pos |> cardButton ( 1, 2 ) (dirMsg Down)
            ]

        NorthAmerica ->
            [ pos |> centerCardButton ( 1, 1 ) centerMsg
            , pos |> cardButton ( 0, 1 ) (dirMsg Left)
            , pos |> cardButton ( 2, 1 ) (dirMsg Right)
            , pos |> cardButton ( 1, 2 ) (dirMsg Down)
            ]


view : { addUnit : msg, swapUnits : msg, removeUnit : msg, resetMove : msg, setMove : Direction -> msg } -> Continent -> { selected : Int, remaining : Int } -> List ( ( Float, Float ), Image msg )
view { addUnit, swapUnits, removeUnit, resetMove, setMove } continent ({ selected, remaining } as selectGui) =
    let
        pos : ( Float, Float )
        pos =
            continent |> View.continentToPosition
    in
    List.concat
        [ [ ( pos, Gui.selectGui selectGui ) ]
        , case remaining of
            1 ->
                []

            _ ->
                pos |> addUnitButton addUnit
        , pos |> swapUnitsButton swapUnits
        , case selected of
            1 ->
                []

            _ ->
                pos |> removeUnitButton removeUnit
        , pos
            |> locationButtons
                continent
                resetMove
                setMove
        ]
