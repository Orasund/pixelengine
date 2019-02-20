module Grid exposing
    ( Grid
    , fill, empty, insert, update, remove
    , isEmpty, member, get, size, dimensions
    , positions, emptyPositions, values, toList, fromList
    , toDict, fromDict
    , map, foldl, foldr, filter, partition, find
    , union, intersect, diff
    )

{-| A `Grid` is a dictionary that has a size constraint.
Here is an example where such a grid is used:
[Snake Example](https://orasund.github.io/pixelengine/#Snake).


# Grids

@docs Grid


# Build

@docs fill, empty, insert, update, remove


# Query

@docs isEmpty, member, get, size, dimensions


# List

@docs positions, emptyPositions, values, toList, fromList


# Dict

@docs toDict, fromDict


# Transform

@docs map, foldl, foldr, filter, partition, find


# Combine

@docs union, intersect, diff

-}

import Dict exposing (Dict)
import Grid.Position exposing (Position)


mapDict : (Dict Position a -> Dict Position b) -> Grid a -> Grid b
mapDict fun (Grid { dict, rows, columns }) =
    Grid
        { dict = fun dict
        , rows = rows
        , columns = columns
        }


wrap : Grid a -> Position -> Position
wrap (Grid { rows, columns }) ( x, y ) =
    ( x |> modBy columns
    , y |> modBy rows
    )



---------------------------------
-- Exposed
---------------------------------


{-| A grid with a fixes amount of columns and rows.

It will wrap the borders (apply ModBy), making every position valid.

    dimensions : { columns:Int , rows:Int }
    dimensions =
        { columns=42
        , rows=3
        }

    grid : Grid a
    grid =
        empty dimensions

    grid |> get ( -1, 0 )
    --> grid |> get ( (dimensions |> .columns) - 1, 0 )

If instead you want to have hard border around your grid, use `Grid.Bordered` instead.

-}
type Grid a
    = Grid
        { dict : Dict Position a
        , rows : Int
        , columns : Int
        }


{-| Create a grid

    dimensions : { columns:Int , rows:Int }
    dimensions =
        { columns=42
        , rows=3
        }

    grid : Grid a
    grid =
        empty dimensions

    fill (always <| Just ()) dimensions |> emptyPositions
    --> []

-}
fill : (Position -> Maybe a) -> { rows : Int, columns : Int } -> Grid a
fill fun config =
    empty config
        |> map (\pos -> always (fun pos))


{-| Create an empty grid

    dimensions : { columns:Int , rows:Int }
    dimensions =
        { columns=42
        , rows=3
        }

    empty dimensions
    --> fill (always Nothing ) dimensions

-}
empty : { rows : Int, columns : Int } -> Grid a
empty { rows, columns } =
    Grid
        { dict = Dict.empty
        , rows = rows
        , columns = columns
        }


{-| Insert a value at a position in a grid. Replaces value when there is a collision.

    dimensions : { columns:Int , rows:Int }
    dimensions =
        { columns=42
        , rows=3
        }

    grid : Grid a
    grid =
        empty dimensions

    grid |> insert (2,2) 42 |> get (2,2)
    --> Just 42

-}
insert : Position -> a -> Grid a -> Grid a
insert pos elem grid =
    grid |> mapDict (Dict.insert (pos |> wrap grid) elem)


{-| Update the value of a grid for a specific position with a given function.

    dimensions : { columns:Int , rows:Int }
    dimensions =
        { columns=42
        , rows=3
        }

    grid : Grid a
    grid =
        empty dimensions

    grid |> update (2,2) (always <| Just 42)
    --> grid |> insert (2,2) 42

-}
update : Position -> (Maybe a -> Maybe a) -> Grid a -> Grid a
update pos fun grid =
    grid |> mapDict (Dict.update (pos |> wrap grid) fun)


{-| Remove a vlaue from a grid. If the position is empty, no changes are made.

    dimensions : { columns:Int , rows:Int }
    dimensions =
        { columns=42
        , rows=3
        }

    grid : Grid a
    grid =
        empty dimensions

    grid |> insert (2,2) 42 |> get (2,2) --> Just 42
    grid |> insert (2,2) 42 |> remove (2,2) |> get (2,2)
    --> Nothing

-}
remove : Position -> Grid a -> Grid a
remove pos grid =
    grid |> mapDict (Dict.remove (pos |> wrap grid))


{-| Determine if a grid is empty.

    dimensions : { columns:Int , rows:Int }
    dimensions =
        { columns=42
        , rows=3
        }

    grid : Grid a
    grid =
        empty dimensions

    grid |> isEmpty --> True
    grid |> insert (2,2) 42 |> isEmpty --> False

-}
isEmpty : Grid a -> Bool
isEmpty (Grid { dict }) =
    dict |> Dict.isEmpty


{-| Determine if a position is empty.

    dimensions : { columns:Int , rows:Int }
    dimensions =
        { columns=42
        , rows=3
        }

    grid : Grid a
    grid =
        empty dimensions

    grid |> insert (2,2) 42 |> member (2,2)
    --> True

-}
member : Position -> Grid a -> Bool
member pos ((Grid { dict }) as grid) =
    dict |> Dict.member (pos |> wrap grid)


{-| Get the value associated with a position. If the position is empty, return Nothing.

    dimensions : { columns:Int , rows:Int }
    dimensions =
        { columns=42
        , rows=3
        }

    grid : Grid a
    grid =
        empty dimensions

    grid |> insert (2,2) 42 |> get (2,2)
    --> Just 42

-}
get : Position -> Grid a -> Maybe a
get pos ((Grid { dict }) as grid) =
    dict |> Dict.get (pos |> wrap grid)


{-| Determine the number of values in the grid.

    dimensions : { columns:Int , rows:Int }
    dimensions =
        { columns=42
        , rows=3
        }

    grid : Grid a
    grid =
        empty dimensions

    grid |> insert (2,2) 42 |> size
    --> 1

-}
size : Grid a -> Int
size (Grid { dict }) =
    dict |> Dict.size


{-| Return the dimensions of the grid.

    dim : { columns:Int , rows:Int }
    dim =
        { columns=42
        , rows=3
        }

    grid : Grid a
    grid =
        empty dim

    grid |> dimensions
    --> dim

-}
dimensions : Grid a -> { columns : Int, rows : Int }
dimensions (Grid { columns, rows }) =
    { columns = columns
    , rows = rows
    }


{-| Get all non empty positions in a grid, sorted from lowest to highest.

    dimensions : { columns:Int , rows:Int }
    dimensions =
        { columns=42
        , rows=3
        }

    grid : Grid a
    grid =
        empty dimensions

    grid |> insert (2,2) 42 |> positions
    --> [(2,2)]

-}
positions : Grid a -> List Position
positions (Grid { dict }) =
    dict |> Dict.keys


{-| Get all of the values in a grid, in the order of their positions.

    dimensions : { columns:Int , rows:Int }
    dimensions =
        { columns=42
        , rows=3
        }

    grid : Grid a
    grid =
        empty dimensions

    grid |> insert (2,2) 42 |> values
    --> [42]

-}
values : Grid a -> List a
values (Grid { dict }) =
    dict |> Dict.values


{-| Get all empty positions in a grid, sorted from lowest to highest.

    dimensions : { columns:Int , rows:Int }
    dimensions =
        { columns=42
        , rows=3
        }

    grid : Grid Int
    grid =
        fill
            (always <| Just <| 42)
            dimensions

    grid |> remove (2,2) |> emptyPositions
    --> [(2,2)]

-}
emptyPositions : Grid a -> List Position
emptyPositions =
    map
        (\_ maybeMark ->
            case maybeMark of
                Just _ ->
                    Nothing

                Nothing ->
                    Just ()
        )
        >> positions


{-| Convert a grid into an association list of position-value pairs,
sorted by the position.

    dimensions : { columns:Int , rows:Int }
    dimensions =
        { columns=42
        , rows=3
        }

    grid : Grid Int
    grid =
        empty dimensions

    grid |> insert (2,2) 42 |> toList
    --> [( (2,2), 42 )]

-}
toList : Grid a -> List ( Position, a )
toList (Grid { dict }) =
    dict |> Dict.toList


{-| Convert an association list into a grid.

    dimensions : { columns:Int , rows:Int }
    dimensions =
        { columns=42
        , rows=3
        }

    grid : Grid Int
    grid =
        empty dimensions

    [((2,2),42),((2,1),20)] |> fromList dimensions
    --> grid |> insert (2,2) 42 |> insert (2,1) 20

-}
fromList : { rows : Int, columns : Int } -> List ( Position, a ) -> Grid a
fromList config =
    List.foldl
        (\( pos, elem ) -> insert pos elem)
        (empty config)


{-| Convert a grid into an associated dictionary

    dimensions : { columns:Int , rows:Int }
    dimensions =
        { columns=42
        , rows=3
        }

    grid : Grid Int
    grid =
        empty dimensions
            |> insert (2,2) 42

    grid |> toDict |> fromDict dimensions |> get (2,2)
    --> Just 42

-}
toDict : Grid a -> Dict Position a
toDict (Grid { dict }) =
    dict


{-| Convert an dictionary to a grid

    dimensions : { columns:Int , rows:Int }
    dimensions =
        { columns=42
        , rows=3
        }

    grid : Grid Int
    grid =
        empty dimensions

    grid |> toDict |> fromDict dimensions |> get (2,2)
    --> Nothing

-}
fromDict : { rows : Int, columns : Int } -> Dict Position a -> Grid a
fromDict config =
    Dict.toList >> fromList config


{-| Apply a function to **all** positions in a grid.

    dimensions : { columns:Int , rows:Int }
    dimensions =
        { columns=42
        , rows=3
        }

    empty dimensions |> map (\_ _ -> Just 42)
    --> fill (always <| Just 42) dimensions

-}
map : (Position -> Maybe a -> Maybe b) -> Grid a -> Grid b
map fun ((Grid { rows, columns }) as grid) =
    grid
        |> foldl
            (\pos elem ->
                case fun pos elem of
                    Just newElem ->
                        insert pos newElem

                    Nothing ->
                        identity
            )
            (empty { rows = rows, columns = columns })


{-| Fold over **all** positions in a grid, row by row, from top down.
-}
foldl : (Position -> Maybe v -> b -> b) -> b -> Grid v -> b
foldl fun val (Grid { dict, rows, columns }) =
    List.range 0 (columns - 1)
        |> List.foldl
            (\x out ->
                List.range 0 (rows - 1)
                    |> List.foldl
                        (\y ->
                            fun ( x, y ) (dict |> Dict.get ( x, y ))
                        )
                        out
            )
            val


{-| Fold over **all** positions in a grid, row by row, from bottum up.
-}
foldr : (Position -> Maybe v -> b -> b) -> b -> Grid v -> b
foldr fun val (Grid { dict, rows, columns }) =
    List.range 0 (columns - 1)
        |> List.foldr
            (\x out ->
                List.range 0 (rows - 1)
                    |> List.foldr
                        (\y ->
                            fun ( x, y ) (dict |> Dict.get ( x, y ))
                        )
                        out
            )
            val


{-| Keep only the values that pass the given test.

    dimensions : { columns:Int , rows:Int }
    dimensions =
        { columns=42
        , rows=3
        }

    grid : Grid Int
    grid =
        empty dimensions
            |> insert (2,4) 2
            |> insert (2,3) 42

    grid |> filter (\_ -> (==) 42) |> values
    --> [42]

-}
filter : (Position -> a -> Bool) -> Grid a -> Grid a
filter fun =
    mapDict (Dict.filter fun)


{-| Partition a grid according to some test.

The first grid contains all values which passed the test,
and the second contains the values that did not.

-}
partition : (Position -> a -> Bool) -> Grid a -> ( Grid a, Grid a )
partition fun ((Grid { dict }) as grid) =
    let
        mapFunction : Dict Position c -> Grid c
        mapFunction d =
            grid |> mapDict (always d)
    in
    dict
        |> Dict.partition fun
        |> Tuple.mapBoth mapFunction mapFunction


{-| Find the first value that passes a given test.
-}
find : (Position -> a -> Bool) -> Grid a -> Maybe ( Position, a )
find fun (Grid { dict }) =
    let
        recFind : List ( Position, a ) -> Maybe ( Position, a )
        recFind list =
            case list of
                (( pos, elem ) as head) :: tail ->
                    if fun pos elem then
                        Just head

                    else
                        recFind tail

                [] ->
                    Nothing
    in
    dict
        |> Dict.toList
        |> recFind


{-| Combine two grids.
If there is a collision, preference is given to the first grid.
-}
union : Grid a -> Grid a -> Grid a
union (Grid { dict, rows, columns }) ((Grid g2) as grid) =
    if rows == g2.rows && columns == g2.columns then
        grid |> mapDict (Dict.union dict)

    else
        grid


{-| Keep a value when its position appears in the second grid.
Preference is given to values in the first grid.
-}
intersect : Grid a -> Grid a -> Grid a
intersect (Grid { dict, rows, columns }) ((Grid g2) as grid) =
    if rows == g2.rows && columns == g2.columns then
        grid |> mapDict (Dict.intersect dict)

    else
        grid


{-| Keep a value when its position does not appear in the second grid.
-}
diff : Grid a -> Grid a -> Grid a
diff (Grid { dict, rows, columns }) ((Grid g2) as grid) =
    if rows == g2.rows && columns == g2.columns then
        grid |> mapDict (Dict.diff dict)

    else
        grid
