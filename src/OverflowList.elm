-- List with overflow


module OverflowList exposing (OverflowList, append, fromList, getList, getMaxLength, getOverflow, length, overflow)


type OverflowList a
    = OverflowList (List a) Int


fromList : Int -> List a -> OverflowList a
fromList max_length list =
    OverflowList list max_length


getList : OverflowList a -> List a
getList (OverflowList list max_length) =
    list
        |> List.take max_length


length : OverflowList a -> Int
length (OverflowList list max_length) =
    list
        |> List.length
        |> min max_length


getMaxLength : OverflowList a -> Int
getMaxLength (OverflowList _ max_length) =
    max_length


overflow : OverflowList a -> Int
overflow (OverflowList list max_length) =
    list
        |> List.length
        |> (-) max_length
        |> max 0


getOverflow : OverflowList a -> Maybe (List a)
getOverflow (OverflowList list max_length) =
    list
        |> List.drop max_length
        |> (\l ->
                if l |> List.isEmpty then
                    Nothing
                else
                    Just l
           )


append : List a -> OverflowList a -> OverflowList a
append a (OverflowList list max_length) =
    OverflowList (a |> List.append list) max_length
