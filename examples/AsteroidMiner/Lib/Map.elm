module AsteroidMiner.Lib.Map exposing (Map, Square, SquareType(..), update)

import AsteroidMiner.Building exposing (Building)
import AsteroidMiner.Data exposing (maxValue)
import AsteroidMiner.Lib.Command as Command exposing (Command)
import Grid.Bordered as Grid exposing (Error(..), Grid)
import Grid.Direction exposing (Direction)
import Grid.Position as Position exposing (Position)


type SquareType a b
    = GroundSquare b
    | BuildingSquare (Building a)


type alias Square a b c =
    ( SquareType a b, Maybe c )


type alias Map a b c =
    Grid (Square a b c)


store : Position -> Building a -> Map a b c -> Result Error (Map a b c)
store pos ({ value } as building) m =
    let
        maybeItem : Maybe c
        maybeItem =
            m
                |> Grid.get pos
                |> Result.withDefault Nothing
                |> Maybe.andThen Tuple.second
    in
    m
        |> Grid.update pos
            (always <|
                if
                    (value < maxValue)
                        && (maybeItem /= Nothing)
                then
                    Ok <|
                        Just <|
                            ( BuildingSquare { building | value = value + 1 }
                            , Nothing
                            )

                else
                    Err ()
            )


send : Position -> Building a -> Maybe c -> { empty : b, lookUp : Map a b c, canStore : Position -> a -> c -> { value : Int, item : c } -> Bool } -> Direction -> Map a b c -> Result Error (Map a b c)
send pos ({ value } as building) maybeItem { lookUp, canStore } direction m =
    let
        neighborPos : Position
        neighborPos =
            pos |> Position.move 1 direction

        updateNeighbor : Maybe c -> Map a b c -> Result Error (Map a b c)
        updateNeighbor maybeC =
            Grid.update neighborPos
                (\maybeSquare ->
                    case maybeSquare of
                        Just ( BuildingSquare b, Nothing ) ->
                            Ok <|
                                Just <|
                                    ( BuildingSquare b, maybeC )

                        _ ->
                            Err ()
                )

        solveConflict : { newC : c, oldC : c } -> Map a b c -> Result Error (Map a b c)
        solveConflict { newC, oldC } =
            Grid.update neighborPos
                (\maybeSquare ->
                    case maybeSquare of
                        Just ( BuildingSquare b, Just _ ) ->
                            if canStore neighborPos b.sort newC { value = b.value, item = oldC } then
                                Ok <|
                                    Just <|
                                        ( BuildingSquare { b | value = b.value + 1 }, Just newC )

                            else
                                Err ()

                        _ ->
                            Err ()
                )
    in
    case maybeItem of
        Nothing ->
            Err NotSuccessful

        Just item ->
            lookUp
                |> Grid.get neighborPos
                |> Result.andThen
                    (\maybeEntry ->
                        m
                            |> (case maybeEntry of
                                    Just ( BuildingSquare _, Nothing ) ->
                                        updateNeighbor maybeItem

                                    Just ( BuildingSquare _, Just i ) ->
                                        solveConflict { newC = item, oldC = i }

                                    _ ->
                                        always <| Err NotSuccessful
                               )
                    )
                |> Result.andThen
                    (Grid.update pos <|
                        always <|
                            let
                                _ =
                                    ( building.sort, maybeItem )
                            in
                            if value > 1 then
                                Ok <|
                                    Just <|
                                        ( BuildingSquare { building | value = value - 1 }
                                        , maybeItem
                                        )

                            else
                                Ok <|
                                    Just <|
                                        ( BuildingSquare { building | value = 0 }, Nothing )
                    )


apply : Command a c -> Position -> Square a b c -> { empty : b, lookUp : Map a b c, canStore : Position -> a -> c -> { value : Int, item : c } -> Bool } -> (Map a b c -> Map a b c)
apply command pos ( squareType, maybeItem ) ({ empty } as config) =
    let
        transition : Building a -> a -> Map a b c -> Result Error (Map a b c)
        transition building sort =
            Grid.update pos <|
                \maybeSquare ->
                    case maybeSquare of
                        Just ( BuildingSquare _, mI ) ->
                            Ok <|
                                Just <|
                                    ( BuildingSquare { building | sort = sort }
                                    , mI
                                    )

                        _ ->
                            Err ()

        create : Building a -> c -> Map a b c -> Result Error (Map a b c)
        create building item =
            Grid.update pos <|
                always <|
                    Ok <|
                        Just <|
                            ( BuildingSquare { building | value = 0 }
                            , Just item
                            )

        destroy : Building a -> Map a b c -> Result Error (Map a b c)
        destroy _ =
            Grid.update pos <|
                \maybeSquare ->
                    case maybeSquare of
                        Just ( BuildingSquare _, mI ) ->
                            Ok <|
                                Just <|
                                    ( GroundSquare empty
                                    , mI
                                    )

                        _ ->
                            Err ()
    in
    case squareType of
        GroundSquare _ ->
            identity

        BuildingSquare building ->
            command
                |> Command.apply
                    { store = store pos building
                    , send = send pos building maybeItem config
                    , transition = transition building
                    , create = create building
                    , destroy = destroy building
                    }


update : { empty : b, update : Position -> Command a c, canStore : Position -> a -> c -> { value : Int, item : c } -> Bool } -> Map a b c -> Map a b c
update fun map =
    map
        |> Grid.foldl
            (\pos maybeSquare ->
                case maybeSquare of
                    Just (( BuildingSquare _, _ ) as square) ->
                        apply (fun.update pos) pos square { empty = fun.empty, lookUp = map, canStore = fun.canStore }

                    _ ->
                        identity
            )
            map
