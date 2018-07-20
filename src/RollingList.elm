module RollingList exposing (RollingList, current, fromList, roll, rollBack, toList)

{-| Module description


## Functions

@docs fromList, roll, rollBack, current, toList


## Definition

@docs RollingList

-}


{-| The rolling list type.
-}
type RollingList a
    = RollingList
        { previous : List a
        , next : List a
        }


{-| Create a rolling list from a normal list

    >>> toList (fromList [1,2,3])
    [1,2,3]

-}
fromList : List a -> RollingList a
fromList l =
    RollingList { previous = [], next = l }


{-| Create a normal list from a rolling list

    >>> toList (fromList [1,2])
    [1,2]

-}
toList : RollingList a -> List a
toList (RollingList { previous, next }) =
    next ++ List.reverse previous


{-| Return a New RollingList, with the current element set to the next element

    >>> toList (roll (fromList [1,2,3]))
    [2,3,1]

    >>> current (roll (fromList [1,2,3]))
    Just 2

-}
roll : RollingList a -> RollingList a
roll (RollingList current) =
    case current.next of
        [] ->
            RollingList { previous = [], next = List.reverse current.previous }

        element :: tail ->
            RollingList { previous = element :: current.previous, next = tail }


{-| Return a New RollingList, with the current element set to the previous element

    >>> toList (roll (fromList [1,2,3]))
    [3,1,2]

    >>> current (roll (fromList [1,2,3]))
    Just 3

-}
rollBack : RollingList a -> RollingList a
rollBack (RollingList current) =
    case current.previous of
        [] ->
            case List.reverse current.next of
                elem :: list ->
                    RollingList { previous = list, next = [ elem ] }

                [] ->
                    RollingList { previous = [], next = [] }

        element :: tail ->
            RollingList { previous = tail, next = element :: current.next }


{-| Returns the currently selected element in the list

    >>> current (fromList [1,2,3])
    Just 1

-}
current : RollingList a -> Maybe a
current (RollingList list) =
    list.next
        |> List.head
