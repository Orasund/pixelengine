module AsteroidMiner.View.Map exposing (view, viewItem, viewSquareType)

import AsteroidMiner.Building as Building exposing (BeltColor(..), BuildingType(..))
import AsteroidMiner.Data.Game as Game
import AsteroidMiner.Data.Item exposing (Item(..))
import AsteroidMiner.Data.Map as Map exposing (GroundType(..), Map, Square)
import AsteroidMiner.Lib.Map exposing (SquareType(..))
import AsteroidMiner.View exposing (ToolSelection(..))
import AsteroidMiner.View.Tileset as Tileset
import Grid.Bordered as Grid
import Grid.Position exposing (Position)
import PixelEngine.Tile as Tile exposing (Tile)


viewSquareType : Map.SquareType -> Tile msg
viewSquareType squareType =
    case squareType of
        GroundSquare groundType ->
            case groundType of
                Dirt ->
                    Tileset.ground

                Mountain ->
                    Tileset.mountain

                OreGround ->
                    Tileset.oreGround

        BuildingSquare buildingType ->
            case buildingType.sort of
                Building.Mine ->
                    Tileset.mine

                Building.ConveyorBelt code ->
                    Tileset.conveyorBelt code

                Building.ColoredConveyorBelt color direction ->
                    Tileset.coloredConveyorBelt color direction

                Building.Container volume ->
                    Tileset.container volume

                Building.Merger ->
                    Tileset.merger

                Building.Sorter ->
                    Tileset.sorter


viewItem : Item -> Tile msg
viewItem item =
    case item of
        Stone ->
            Tileset.stone


viewSquare : { position : Position, onClick : Position -> msg, valid : Maybe Bool } -> Square -> Tile msg
viewSquare { position, onClick, valid } ( squareType, maybeItem ) =
    let
        item : Maybe (Tile msg)
        item =
            maybeItem |> Maybe.map viewItem
    in
    (case item of
        Just tile ->
            Tile.multipleTiles
                [ viewSquareType squareType, tile ]

        Nothing ->
            viewSquareType squareType
    )
        |> (case valid of
                Just bool ->
                    \t ->
                        if bool then
                            Tile.multipleTiles [ t, Tileset.valid ]
                                |> Tile.clickable (onClick position)

                        else
                            t

                Nothing ->
                    Tile.clickable (onClick position)
           )


view : { onClick : Position -> msg, selected : ToolSelection, inventory : Int } -> Map -> List ( Position, Tile msg )
view { onClick, selected } map =
    map
        |> Grid.map
            (\pos maybeSquare ->
                case ( maybeSquare, Game.isValid selected pos map ) of
                    ( Just square, valid ) ->
                        square
                            |> viewSquare
                                { position = pos
                                , onClick = onClick
                                , valid = Just valid
                                }
                            |> Just

                    ( Nothing, True ) ->
                        Just (Tileset.valid |> Tile.clickable (onClick pos))

                    _ ->
                        Nothing
            )
        >> Grid.toList
