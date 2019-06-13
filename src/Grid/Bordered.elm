module Grid.Bordered exposing
    ( Error, ignoringErrors
    , Grid
    , fill, empty, insert, update, remove
    , isEmpty, member, get, isValid, size, dimensions
    , positions, emptyPositions, values, toList, fromList
    , toDict, fromDict
    , map, foldl, foldr, filter, partition, find
    , union, intersect, diff
    )

{-| DEPRECATED!
Got moved to its [separate package](https://package.elm-lang.org/packages/Orasund/elm-game-essentials/latest/).

A grid with a hard border around the edges. you cant read or write from squares
outside the border.
Here is an example where such a grid is used:
[Space Invaders](https://orasund.github.io/pixelengine/#SpaceInvaders).


# Error

@docs Error, ignoringErrors


# Grids

@docs Grid


# Build

@docs fill, empty, insert, update, remove


# Query

@docs isEmpty, member, get, isValid, size, dimensions


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



---------------------------------
-- Exposed
---------------------------------


{-| Possible Errors.

  - `OutOfBounds` - the position is not valid
  - `NotSucessful` - the function as a problem fullfilling the task. (Maybe the position is occupied?)

-}
type Error
    = OutOfBounds
    | NotSuccessful


{-| A grid of values.

It has a fixed amount of columns and rows.

If case of a invalid position, it returns an error.

    grid |> Grid.insert ( -1, 0 ) virus == Err OutOfBounds

-}
type Grid a
    = Grid
        { dict : Dict Position a
        , rows : Int
        , columns : Int
        }


{-| checks if a position is valid.

    grid : Grid a
    grid =
        empty
            { columns=42
            , rows=3
            }


    grid |> isValid (-1,0) --> False
    grid |> isValid (41,0) --> True
    grid |> isValid (42,0) --> False

-}
isValid : Position -> Grid a -> Bool
isValid ( x, y ) (Grid { rows, columns }) =
    x >= 0 && x < columns && y >= 0 && y < rows


{-| Create a grid
-}
fill : (Position -> Maybe a) -> { rows : Int, columns : Int } -> Grid a
fill fun config =
    empty config
        |> map (\pos -> always (fun pos))


{-| Create an empty grid
-}
empty : { rows : Int, columns : Int } -> Grid a
empty { rows, columns } =
    Grid
        { dict = Dict.empty
        , rows = rows
        , columns = columns
        }


{-| Insert a value at a position in a grid. Returns a `NotSuccessful` error if
the position is occupied.
-}
insert : Position -> a -> Grid a -> Result Error (Grid a)
insert pos elem ((Grid { dict }) as grid) =
    if grid |> isValid pos then
        if dict |> Dict.member pos then
            Err NotSuccessful

        else
            Ok <| mapDict (Dict.insert pos elem) grid

    else
        Err OutOfBounds


{-| Update the value of a grid for a specific position with a given function.
-}
update : Position -> (Maybe a -> Result () (Maybe a)) -> Grid a -> Result Error (Grid a)
update pos fun ((Grid { dict }) as grid) =
    if grid |> isValid pos then
        case dict |> Dict.get pos |> fun of
            Ok result ->
                Ok <|
                    mapDict
                        (case result of
                            Just a ->
                                Dict.insert pos a

                            Nothing ->
                                Dict.remove pos
                        )
                        grid

            Err () ->
                Err NotSuccessful

    else
        Err OutOfBounds


{-| Remove a vlaue from a grid. If the position is empty, it returs a
`NotSuccessful` error.
-}
remove : Position -> Grid a -> Result Error (Grid a)
remove pos ((Grid { dict }) as grid) =
    if grid |> isValid pos then
        if dict |> Dict.member pos then
            Ok <| mapDict (Dict.remove pos) grid

        else
            Err NotSuccessful

    else
        Err OutOfBounds


{-| Trys modifying a grid and if its fails it returns the original grid.

Please be aware, that by using this function you might introduce errors.

-}
ignoringErrors : (Grid a -> Result result (Grid a)) -> Grid a -> Grid a
ignoringErrors fun grid =
    case grid |> fun of
        Ok result ->
            result

        Err _ ->
            grid


{-| Determine if a grid is empty.
-}
isEmpty : Grid a -> Bool
isEmpty (Grid { dict }) =
    dict |> Dict.isEmpty


{-| Determine if a position is empty.
-}
member : Position -> Grid a -> Result () Bool
member pos ((Grid { dict }) as grid) =
    if grid |> isValid pos then
        dict |> Dict.member pos |> Ok

    else
        Err ()


{-| Get the value associated with a position. If the position is empty, return Nothing.
-}
get : Position -> Grid a -> Result () (Maybe a)
get pos ((Grid { dict }) as grid) =
    if grid |> isValid pos then
        dict |> Dict.get pos |> Ok

    else
        Err ()


{-| Determine the number of values in the grid.
-}
size : Grid a -> Int
size (Grid { dict }) =
    dict |> Dict.size


{-| return the dimensions of the grid
-}
dimensions : Grid a -> { columns : Int, rows : Int }
dimensions (Grid { columns, rows }) =
    { columns = columns
    , rows = rows
    }


{-| Get all non empty positions in a grid, sorted from lowest to highest.
-}
positions : Grid a -> List Position
positions (Grid { dict }) =
    dict |> Dict.keys


{-| Get all empty positions in a grid, sorted from lowest to highest.
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


{-| Get all of the values in a grid, in the order of their keys.
-}
values : Grid a -> List a
values (Grid { dict }) =
    dict |> Dict.values


{-| Convert a grid into an association list of position-value pairs, sorted by the position.
-}
toList : Grid a -> List ( Position, a )
toList (Grid { dict }) =
    dict |> Dict.toList


{-| Convert an association list into a grid.
-}
fromList : { rows : Int, columns : Int } -> List ( Position, a ) -> Grid a
fromList config =
    List.foldl
        (\( pos, elem ) grid ->
            case grid |> insert pos elem of
                Ok value ->
                    value

                Err _ ->
                    grid
        )
        (empty config)


{-| Convert a grid into an associated dictionary
-}
toDict : Grid a -> Dict Position a
toDict (Grid { dict }) =
    dict


{-| Convert an dictionary to a grid
-}
fromDict : { rows : Int, columns : Int } -> Dict Position a -> Grid a
fromDict config =
    Dict.toList >> fromList config


{-| Apply a function to **all** positions in a grid.
-}
map : (Position -> Maybe a -> Maybe b) -> Grid a -> Grid b
map fun ((Grid { rows, columns }) as grid) =
    grid
        |> foldl
            (\pos elem g ->
                case fun pos elem of
                    Just newElem ->
                        case g |> insert pos newElem of
                            Ok value ->
                                value

                            Err _ ->
                                g

                    Nothing ->
                        g
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
