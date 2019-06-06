module Spaceinvaders exposing (main)

import Grid.Bordered as Grid exposing (Grid)
import Grid.Direction as Direction exposing (Direction(..))
import Grid.Position as Position exposing (Coord, Position)
import PixelEngine exposing (Area, Input(..), PixelEngine, game)
import PixelEngine.Options as Options exposing (Options)
import PixelEngine.Tile as Tile exposing (Tile)
import Random exposing (Generator, Seed)
import Time



{------------------------
   TYPES
------------------------}


type EntityType
    = Invader
    | Bullet


type alias Entity =
    { direction : Direction
    , entityType : EntityType
    }


type alias State =
    { board : Grid Entity
    , player : Position
    }


type alias Model =
    Maybe ( State, Seed )


type Msg
    = Move Direction
    | SetSeed Seed
    | Shoot
    | Tick



{------------------------
   GLOBAL VARIABLES
------------------------}


boardSize : Int
boardSize =
    6


tileSize : Int
tileSize =
    8


width : Float
width =
    toFloat <| (boardSize + 2) * tileSize


offset : Coord
offset =
    { x = 1
    , y = 1
    }



{------------------------
   INIT
------------------------}


init : () -> ( Model, Cmd Msg )
init _ =
    ( Nothing
    , Random.generate SetSeed Random.independentSeed
    )



{------------------------
   UPDATE
------------------------}


newBoard : Grid Entity
newBoard =
    Grid.fill
        (\( x, y ) ->
            if x > 2 && y < 3 then
                Just
                    { direction = Left
                    , entityType = Invader
                    }

            else
                Nothing
        )
        { columns = boardSize
        , rows = boardSize
        }


movePlayer : Direction -> State -> State
movePlayer direction ({ player, board } as state) =
    let
        newPosition =
            player |> Position.move 1 direction
    in
    if board |> Grid.isValid newPosition then
        { state
            | player = newPosition
            , board = board |> checkIfGameOver newPosition
        }

    else
        state


{-| if the player shoots, we want to destroy the lowest enemy on the y axis.
Once we have found the target enemy we will remove it. We can ignore the Errors
because we know that this can not return an error.
-}
shoot : State -> State
shoot ({ player, board } as state) =
    let
        ( playerX, _ ) =
            player
    in
    case
        board
            |> Grid.filter
                (\( x, _ ) { entityType } ->
                    (entityType == Invader)
                        && (x == playerX)
                )
            |> Grid.positions
            |> List.reverse
    of
        pos :: _ ->
            { state
                | board =
                    board
                        |> Grid.ignoringErrors (Grid.remove pos)
            }

        [] ->
            state


{-| when updating the Entity, we need to read from the old grid (`board`) and read
into the new grid (in our case, we only return the function. the new grid will then
apply the functin.)
-}
updateEntity : Grid Entity -> Position -> Maybe Entity -> Grid Entity -> Grid Entity
updateEntity board pos maybeEntity =
    case maybeEntity of
        Just ({ direction, entityType } as entity) ->
            let
                newPos : Position
                newPos =
                    pos |> Position.move 1 direction

                changeDirection : Grid Entity -> Grid Entity
                changeDirection =
                    Grid.ignoringErrors
                        (Grid.update pos
                            (always <|
                                Ok <|
                                    Just
                                        { direction =
                                            direction
                                                |> Direction.flip
                                        , entityType = entityType
                                        }
                            )
                        )

                move : Grid Entity -> Grid Entity
                move =
                    Grid.ignoringErrors
                        (Grid.insert newPos entity
                            >> Result.andThen (Grid.remove pos)
                        )
            in
            case entityType of
                Invader ->
                    case board |> Grid.get newPos of
                        Ok result ->
                            case result of
                                Nothing ->
                                    move

                                Just _ ->
                                    changeDirection

                        Err () ->
                            changeDirection

                Bullet ->
                    case board |> Grid.get newPos of
                        Ok _ ->
                            move

                        Err () ->
                            Grid.ignoringErrors
                                (Grid.remove pos)

        Nothing ->
            identity


{-| the bullets should spawn randomly. This can be done by defining a Generator.
It takes a grid and then randomly inserts bullets.

First we find all possible `spawnPositions`. Next get a list of same length that
with a bool representing whether a bullet will spawn at a give positon or not.

-}
bulletGenerator : Grid Entity -> Generator (Grid Entity)
bulletGenerator g =
    let
        spawnPositions : List Position
        spawnPositions =
            g
                |> Grid.filter
                    (\pos { entityType } ->
                        (entityType == Invader)
                            && (g
                                    |> Grid.get (pos |> Position.move 1 Down)
                                    |> Result.map ((==) Nothing)
                                    |> Result.withDefault False
                               )
                    )
                |> Grid.positions
    in
    Random.map
        (List.map2 (\a b -> ( a, b )) spawnPositions
            >> List.foldl
                (\( pos, bool ) ->
                    if bool then
                        Grid.ignoringErrors
                            (Grid.insert (pos |> Position.move 1 Down)
                                { direction = Down, entityType = Bullet }
                            )

                    else
                        identity
                )
                g
        )
        (Random.list (spawnPositions |> List.length)
            (Random.weighted ( 1, True ) [ ( 2, False ) ])
        )


checkIfGameOver : Position -> Grid Entity -> Grid Entity
checkIfGameOver player grid =
    if grid |> Grid.member player |> Result.withDefault True then
        newBoard

    else
        grid


{-| This is the way how i like to handle random events.
-}
updateState : ( State, Seed ) -> ( State, Seed )
updateState ( { board, player } as state, seed ) =
    let
        boardGenerator : Generator (Grid Entity)
        boardGenerator =
            board
                |> Grid.foldl
                    (updateEntity board)
                    board
                |> bulletGenerator

        stateGenerator : Generator State
        stateGenerator =
            boardGenerator
                |> Random.map
                    (\grid ->
                        { state
                            | board =
                                grid
                                    |> checkIfGameOver player
                        }
                    )
    in
    Random.step stateGenerator seed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case model of
        Just ( state, seed ) ->
            case msg of
                Move direction ->
                    Just ( state |> movePlayer direction, seed )

                Tick ->
                    Just (( state, seed ) |> updateState)

                Shoot ->
                    Just (( state |> shoot, seed ) |> updateState)

                SetSeed _ ->
                    model

        Nothing ->
            case msg of
                SetSeed seed ->
                    Just
                        ( { board = newBoard
                          , player = ( boardSize // 2, boardSize - 1 )
                          }
                        , seed
                        )

                _ ->
                    model
    , Cmd.none
    )



{------------------------
   SUBSCRIPTIONS
------------------------}


subscriptions : Model -> Sub Msg
subscriptions _ =
    let
        second : Float
        second =
            1000
    in
    Time.every (second * 1) (always Tick)



{------------------------
   CONTROLS
------------------------}


controls : Input -> Maybe Msg
controls input =
    case input of
        InputA ->
            Just <| Shoot

        InputLeft ->
            Just <| Move Left

        InputRight ->
            Just <| Move Right

        _ ->
            Nothing



{------------------------
   VIEW
------------------------}


playerTile : Tile Msg
playerTile =
    Tile.fromPosition ( 1, 0 )


bulletTile : Tile Msg
bulletTile =
    Tile.fromPosition ( 0, 1 )


invaderTile : Tile Msg
invaderTile =
    Tile.fromPosition ( 1, 1 )


areas : State -> List (Area Msg)
areas { board, player } =
    [ PixelEngine.tiledArea
        { rows = boardSize + 2
        , tileset =
            { source = "tileset.png"
            , spriteWidth = tileSize
            , spriteHeight = tileSize
            }
        , background =
            PixelEngine.imageBackground
                { height = width
                , width = width
                , source = "background.png"
                }
        }
        (board
            |> Grid.toList
            |> List.map
                (\( pos, { entityType } ) ->
                    ( pos |> Position.add offset
                    , case entityType of
                        Invader ->
                            invaderTile

                        Bullet ->
                            bulletTile
                    )
                )
            |> (::) ( player |> Position.add offset, playerTile )
        )
    ]



{------------------------
   CONFIGURATION
------------------------}


options : Options Msg
options =
    Options.default
        |> Options.withMovementSpeed 0.8


view :
    Model
    -> { title : String, options : Maybe (Options Msg), body : List (Area Msg) }
view model =
    { title = "Space Invaders"
    , options = Just options
    , body =
        case model of
            Just ( state, _ ) ->
                areas state

            Nothing ->
                []
    }


main : PixelEngine () Model Msg
main =
    game
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , controls = controls
        , width = width
        }
