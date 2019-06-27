module AsteroidMiner.View.GUI exposing (Model, Msg, init, select, toDefault, update, view)

import AsteroidMiner.Data exposing (floorCosts, maxValue, mineVolume, size, spriteSize)
import AsteroidMiner.Data.Item exposing (Item)
import AsteroidMiner.View exposing (ToolSelection(..))
import AsteroidMiner.View.Inventory as Inventory
import AsteroidMiner.View.Map as Map
import AsteroidMiner.View.Tileset exposing (font)
import AsteroidMiner.View.Tileset.Big as Tileset
import Location exposing (Location)
import PixelEngine.Image as Image exposing (Image)


type alias Model =
    { selected : ToolSelection
    }


type Msg
    = ItemSelected ToolSelection


init : Model
init =
    { selected = Bag Nothing
    }


select : ToolSelection -> Model -> Model
select tool model =
    { model | selected = tool }


toDefault : Model -> Model
toDefault =
    always init


update : Msg -> Model -> Model
update msg model =
    case msg of
        ItemSelected blueprint ->
            { model | selected = blueprint }


viewList : List (Image msg) -> List ( Location, Image msg )
viewList list =
    let
        length : Float
        length =
            list
                |> List.length
                |> toFloat

        center : Float
        center =
            toFloat size * spriteSize / 2
    in
    list
        |> List.indexedMap
            (\i image ->
                ( ( center
                        - (length * spriteSize)
                        + (toFloat i * spriteSize * 2)
                  , 0
                  )
                , image
                )
            )


viewBlueprint : ToolSelection -> ToolSelection -> Image Msg
viewBlueprint selected blueprint =
    let
        { image, symobl } =
            case blueprint of
                Mine ->
                    Tileset.mine

                ConveyorBelt ->
                    Tileset.conveyorBelt

                Container ->
                    Tileset.container

                Delete ->
                    Tileset.delete

                Bag bag ->
                    bag
                        |> Maybe.map Map.viewItem
                        |> Tileset.pickUp

                Merger ->
                    Tileset.merger

                Sorter ->
                    Tileset.sorter

                Floor ->
                    Tileset.floor
    in
    if blueprint == selected then
        image

    else
        symobl
            |> Image.clickable (ItemSelected blueprint)


viewDesc : ToolSelection -> List ( Location, Image Msg )
viewDesc selected =
    let
        text : String
        text =
            case selected of
                Mine ->
                    "Mine - Mines " ++ String.fromInt mineVolume ++ " items"

                ConveyorBelt ->
                    "Conveyor Belt - Transports items"

                Container ->
                    "Container - Stores " ++ String.fromInt maxValue ++ " items"

                Delete ->
                    "DELETE BUILDINGS"

                Bag _ ->
                    "PICK UP ITEMS"

                Merger ->
                    "Merger - Takes from containers"

                Sorter ->
                    "Sorter - Sorts into containers"

                Floor ->
                    "Floor - Costs " ++ String.fromInt floorCosts ++ " items"
    in
    [ ( ( 0, (toFloat <| 2) * spriteSize ), Image.fromText text font ) ]


view : Maybe Item -> Int -> Model -> List ( Location, Image Msg )
view bag inventory { selected } =
    List.concat
        [ [ Bag bag
          , Mine
          , ConveyorBelt
          , Container
          , Merger
          , Sorter
          , Floor
          , Delete
          ]
            |> List.map (viewBlueprint selected)
            |> viewList
        , viewDesc selected
        , Inventory.view inventory
        ]
