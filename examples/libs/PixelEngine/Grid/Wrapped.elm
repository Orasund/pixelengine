module PixelEngine.Grid.Wrapped exposing (WrappedGrid
        ,fill,empty,insert,update,remove
        ,isEmpty,member,get,size,dimensions
        ,keys, values, toList,fromList
        ,toDict,fromDict
        ,map,foldl,foldr,filter,partition,find
    )

import Dict exposing (Dict)
import PixelEngine.Grid.Position exposing (Position)

{-| A `Grid` is a dictionary that has a size constraint.

# Grids
@docs Grid

# Build
@docs fill,empty,insert,update,remove

# Query
@docs isEmpty,member,get,size,dimensions

# List
@docs keys, values, toList,fromList

# Dict
@docs toDict,fromDict

#Transform
@docs map,foldl,foldr,filter,partition,find
-}
isValid : Position -> WrappedGrid a -> Bool
isValid ( x, y ) (WrappedGrid { rows, columns }) =
    x >= 0 && x < columns && y >= 0 && y < rows


mapDict : (Dict Position a -> Dict Position b) -> WrappedGrid a -> WrappedGrid b
mapDict fun (WrappedGrid { dict, rows, columns}) =
    WrappedGrid
        { dict = fun dict
        , rows = rows
        , columns = columns
        }


wrap : WrappedGrid a -> Position -> Position
wrap (WrappedGrid { rows, columns }) ( x, y ) =
    ( x |> modBy columns
    , y |> modBy rows
    )



---------------------------------
-- Exposed
---------------------------------

{-| A wrapped grid of values.

It has a fixed amount of columns and rows.

It will wrap the borders (apply ModBy), making every position valid.
```
grid |> Dict.get (-1,0) == grid |> Grid.get (columns-1,0)
```
-}
type WrappedGrid a
    = WrappedGrid
        { dict : Dict Position a
        , rows : Int
        , columns : Int
        }

{-| Create a grid
-}
fill : (Position -> Maybe a) -> { rows : Int, columns : Int } -> WrappedGrid a
fill fun config =
    empty config
        |> map (\pos -> always (fun pos))

{-| Create an empty grid
-}
empty : { rows : Int, columns : Int} -> WrappedGrid a
empty { rows, columns } =
    WrappedGrid
        { dict = Dict.empty
        , rows = rows
        , columns = columns
        }

{-| Insert a value at a position in a grid. Replaces value when there is a collision.
-}
insert : Position -> a -> WrappedGrid a -> (WrappedGrid a)
insert pos elem grid =
    grid |> mapDict (Dict.insert (pos |> wrap grid) elem)

{-| Update the value of a grid for a specific position with a given function.
-}
update : Position -> (Maybe a -> Maybe a) -> WrappedGrid a -> (WrappedGrid a)
update pos fun (grid) =
    grid |> mapDict (Dict.update (pos |> wrap grid) fun)

{-| Remove a vlaue from a grid. If the position is empty, no changes are made.
-}
remove : Position -> WrappedGrid a -> (WrappedGrid a)
remove pos (grid) =
    grid |> mapDict (Dict.remove (pos |> wrap grid))

{-| Determine if a grid is empty.
-}
isEmpty : WrappedGrid a -> Bool
isEmpty (WrappedGrid { dict }) =
    dict |> Dict.isEmpty

{-| Determine if a position is empty.
-}
member : Position -> WrappedGrid a -> Bool
member pos ( (WrappedGrid { dict })  as grid) =
    dict |> Dict.member (pos |> wrap grid)

{-| Get the value associated with a position. If the position is empty, return Nothing.
-}
get : Position -> WrappedGrid a -> Maybe a
get pos ((WrappedGrid { dict }) as grid) =
    dict |> Dict.get (pos |> wrap grid)

{-| Determine the number of values in the grid.
-}
size : WrappedGrid a -> Int
size ((WrappedGrid { dict }) as grid) =
    dict |> Dict.size

{-| return the dimensions of the grid
-}
dimensions : WrappedGrid a -> {columns:Int,rows:Int}
dimensions (WrappedGrid { columns,rows }) =
    {columns=columns
    ,rows =rows
    }

{-| Get all non empty positions in a grid, sorted from lowest to highest.
-}
keys : WrappedGrid a -> List Position
keys ((WrappedGrid { dict }) as grid) =
    dict |> Dict.keys

{-| Get all of the values in a grid, in the order of their keys.
-}
values : WrappedGrid a -> List a
values ((WrappedGrid { dict }) as grid) =
    dict |> Dict.values

{-| Convert a grid into an association list of position-value pairs, sorted by the position.
-}
toList : WrappedGrid a -> List ( Position, a )
toList ((WrappedGrid { dict }) as grid) =
    dict |> Dict.toList

{-| Convert an association list into a grid.
-}
fromList : { rows : Int, columns : Int} -> List ( Position, a ) -> WrappedGrid a
fromList config =
    List.foldl
        (\( pos, elem ) -> insert pos elem)
        (empty config)

{-| Convert a grid into an associated dictionary
-}
toDict : WrappedGrid a -> Dict Position a
toDict (WrappedGrid { dict }) =
    dict

{-| Convert an dictionary to a grid
-}
fromDict : { rows : Int, columns : Int } -> Dict Position a -> WrappedGrid a
fromDict config =
    Dict.toList >> fromList config

{-| Apply a function to all values in a grid.
-}
map : (Position -> Maybe a -> Maybe b) -> WrappedGrid a -> WrappedGrid b
map fun ((WrappedGrid { dict, rows, columns }) as grid) =
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



{-| Fold over all positions in a grid, row by row, from top down.
-}
foldl : (Position -> Maybe v -> b -> b) -> b -> WrappedGrid v -> b
foldl fun val ((WrappedGrid { dict, rows, columns}) as grid) =
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



{-| Fold over all positions in a grid, row by row, from bottum up.
-}
foldr : (Position -> Maybe v -> b -> b) -> b -> WrappedGrid v -> b
foldr fun val ((WrappedGrid { dict, rows, columns }) as grid) =
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
-}
filter : (Position -> a -> Bool) -> WrappedGrid a -> WrappedGrid a
filter fun =
    mapDict (Dict.filter fun)

{-| Partition a grid according to some test.

The first grid contains all values which passed the test,
and the second contains the values that did not.
-}
partition : (Position -> a -> Bool) -> WrappedGrid a -> ( WrappedGrid a, WrappedGrid a )
partition fun ((WrappedGrid { dict }) as grid) =
    let
        mapFunction : Dict Position c -> WrappedGrid c
        mapFunction d =
            grid |> mapDict (always d)
    in
    dict
        |> Dict.partition fun
        |> Tuple.mapBoth mapFunction mapFunction

{-| Find the first value that passes a given test.
-}
find : (Position -> a -> Bool) -> WrappedGrid a -> Maybe ( Position, a )
find fun ((WrappedGrid { dict }) as grid) =
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
