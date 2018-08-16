module DigDigBoom.Component.GroupedList
    exposing
        ( GroupedList
        , add
        , fromList
        , head
        , reduce
        , rotateLeft
        , rotateRight
        , toList
        )

import RollingList exposing (RollingList)


type alias GroupedList a =
    RollingList ( a, Int )


fromList : List a -> GroupedList a
fromList list =
    list
        |> List.foldr
            (\elem base_list ->
                case base_list of
                    ( a, b ) :: c ->
                        if a == elem then
                            ( elem, b + 1 ) :: c
                        else
                            ( elem, 1 ) :: base_list

                    b ->
                        ( elem, 1 ) :: b
            )
            []
        |> RollingList.fromList


toList : GroupedList a -> List a
toList list =
    list
        |> RollingList.toList
        |> List.concatMap
            (\( elem, num ) ->
                List.repeat num elem
            )


rotateLeft : GroupedList a -> GroupedList a
rotateLeft list =
    list |> RollingList.rollBack


rotateRight : GroupedList a -> GroupedList a
rotateRight list =
    list |> RollingList.roll


add : a -> GroupedList a -> GroupedList a
add target l =
    l
        |> RollingList.toList
        |> List.foldr
            (\elem ( list, found ) ->
                if found == False then
                    if (elem |> Tuple.first) == target then
                        ( list
                            |> List.append [ ( target, (elem |> Tuple.second) + 1 ) ]
                        , True
                        )
                    else
                        ( list |> List.append [ elem ], False )
                else
                    ( list |> List.append [ elem ], True )
            )
            ( [], False )
        |> (\( list, found ) ->
                if found == False then
                    List.append list [ ( target, 1 ) ]
                else
                    list
           )
        |> RollingList.fromList


reduce : a -> GroupedList a -> GroupedList a
reduce target l =
    l
        |> RollingList.toList
        |> List.foldr
            (\elem ( list, found ) ->
                if found == False then
                    if (elem |> Tuple.first) == target then
                        if (elem |> Tuple.second) - 1 <= 0 then
                            ( list, True )
                        else
                            ( list
                                |> List.append [ ( target, (elem |> Tuple.second) - 1 ) ]
                            , True
                            )
                    else
                        ( list |> List.append [ elem ], False )
                else
                    ( list |> List.append [ elem ], True )
            )
            ( [], False )
        |> Tuple.first
        |> RollingList.fromList


head : GroupedList a -> Maybe ( a, Int )
head list =
    list
        |> RollingList.current
