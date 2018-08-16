module DigDigBoom.Component.Inventory
    exposing
        ( Inventory
        , add
        , drop
        , get
        , ground
        , init
        , rotateLeft
        , rotateRight
        , selected
        , take
        )

import DigDigBoom.Component.GroupedList as GroupedList
import DigDigBoom.Component.GroupedOverflowList as GroupedOverflowList exposing (GroupedOverflowList)


type alias Inventory a =
    GroupedOverflowList a


init : Int -> Inventory a
init max_length =
    GroupedOverflowList.empty max_length


add : a -> Inventory a -> Inventory a
add item inventory =
    inventory
        |> GroupedOverflowList.setOverflow (Just item)


rotateLeft : Inventory a -> Inventory a
rotateLeft inventory =
    inventory
        |> GroupedOverflowList.getList
        |> GroupedList.rotateLeft
        |> GroupedOverflowList.fromList
            (inventory |> GroupedOverflowList.getMaxLength)
        |> GroupedOverflowList.setOverflow
            (inventory |> GroupedOverflowList.getOverflow)


rotateRight : Inventory a -> Inventory a
rotateRight inventory =
    inventory
        |> GroupedOverflowList.getList
        |> GroupedList.rotateRight
        |> GroupedOverflowList.fromList
            (inventory |> GroupedOverflowList.getMaxLength)
        |> GroupedOverflowList.setOverflow
            (inventory |> GroupedOverflowList.getOverflow)


drop : Inventory a -> ( Maybe a, Inventory a )
drop inventory =
    ( inventory |> GroupedOverflowList.getOverflow
    , inventory |> GroupedOverflowList.setOverflow Nothing
    )


take : Inventory a -> ( Maybe a, Inventory a )
take inventory =
    ( inventory
        |> GroupedOverflowList.getList
        |> GroupedList.head
        |> Maybe.map Tuple.first
    , inventory
        |> GroupedOverflowList.getList
        |> (\list ->
                case list |> GroupedList.head of
                    Just ( a, _ ) ->
                        list
                            |> GroupedList.reduce a

                    Nothing ->
                        list
           )
        |> (\list ->
                inventory
                    |> GroupedOverflowList.getOverflow
                    |> Maybe.map (\a -> list |> GroupedList.add a)
                    |> Maybe.withDefault list
           )
        |> GroupedOverflowList.fromList (inventory |> GroupedOverflowList.getMaxLength)
    )


get : Inventory a -> List a
get inventory =
    inventory
        |> GroupedOverflowList.getList
        |> GroupedList.toList


ground : Inventory a -> Maybe a
ground inventory =
    inventory
        |> GroupedOverflowList.getOverflow


selected : Inventory a -> Maybe a
selected inventory =
    inventory
        |> GroupedOverflowList.getList
        |> GroupedList.head
        |> Maybe.map Tuple.first
