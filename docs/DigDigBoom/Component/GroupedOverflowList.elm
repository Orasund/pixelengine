module DigDigBoom.Component.GroupedOverflowList
    exposing
        ( GroupedOverflowList
        , empty
        , fromList
        , getList
        , getMaxLength
        , getOverflow
        , length
        , setOverflow
        )

import DigDigBoom.Component.GroupedList as GroupedList exposing (GroupedList)
import OverflowList exposing (OverflowList)


type alias GroupedOverflowList a =
    OverflowList a


empty : Int -> GroupedOverflowList a
empty max_length =
    OverflowList.fromList max_length []


length : GroupedOverflowList a -> Int
length list =
    list |> OverflowList.getList |> List.length


getMaxLength : GroupedOverflowList a -> Int
getMaxLength list =
    list |> OverflowList.getMaxLength


getList : GroupedOverflowList a -> GroupedList a
getList list =
    list
        |> OverflowList.getList
        |> GroupedList.fromList


fromList : Int -> GroupedList a -> GroupedOverflowList a
fromList size list =
    list
        |> GroupedList.toList
        |> OverflowList.fromList size


getOverflow : GroupedOverflowList a -> Maybe a
getOverflow list =
    list
        |> OverflowList.getOverflow
        |> Maybe.andThen List.head


setOverflow : Maybe a -> GroupedOverflowList a -> GroupedOverflowList a
setOverflow item list =
    let
        setAsOverflow : Maybe a -> GroupedOverflowList a -> GroupedOverflowList a
        setAsOverflow a l =
            a
                |> Maybe.map List.singleton
                |> Maybe.withDefault []
                |> List.append (l |> OverflowList.getList)
                |> OverflowList.fromList (l |> getMaxLength)
    in
    case list |> OverflowList.getOverflow of
        Just (_ :: _) ->
            setAsOverflow item list

        _ ->
            case item of
                Just a ->
                    if (list |> length) < (list |> getMaxLength) then
                        list
                            |> getList
                            |> GroupedList.add a
                            |> fromList (list |> getMaxLength)
                    else
                        setAsOverflow item list

                Nothing ->
                    list
                        |> getList
                        |> fromList (list |> getMaxLength)
