module Snake exposing (main)

import Array exposing (Array)
import Grid as Grid exposing (Grid)
import Grid.Direction exposing (Direction(..))
import Grid.Position as Position exposing (Coord, Position)
import PixelEngine exposing (Area, Background, Input(..), PixelEngine, game)
import PixelEngine.Options as Options exposing (Options)
import PixelEngine.Tile as Tile exposing (Tile, Tileset)
import Random
import Time



{------------------------
   TYPES
------------------------}


{-| The snake itself has a head as well as additional squares for the body.
A main concept of Elm is trying to define your models in such a way that it can
not allow impossible states. For example if we had used just a `List (Int,Int)`
for the snake, we would always need to also consider the empty list, which can
actually never occur.
-}
type alias Snake =
    ( Position, List Position )


{-|


# Model

We will let the player only have control of the direction, the snake looks,
the movement will be done with a timer. The chicken is a `Maybe Position`
because for a few frames, between it got eaten and a new one is spawned in,
it actually does not exist. If we would have just used a `Position` it would
have also worked, but it might cause some visual glitch.

-}
type alias Model =
    { direction : Direction
    , snake : Snake
    , chicken : Maybe Position
    }


{-|


# Actions

There are Three things that can happen in the Game:

  - The player changes the direction (`Look`)
  - A new chicken gets placed (`PlaceChicken`)
  - The snake moves (`Move`)

-}
type Msg
    = Look Direction
    | PlaceChicken Position
    | Move



{------------------------
   GLOBAL VARIABLES
------------------------}


boardSize : Int
boardSize =
    6


tileSize : Int
tileSize =
    32


width : Float
width =
    toFloat <| (boardSize + 2) * tileSize



{------------------------
   INIT
------------------------}


{-| If you never have worked with
[elm/random](https://package.elm-lang.org/packages/elm/random/latest/) before,
this might look very frightening, so here are the functions in detail:

  - `Random.generate` - creates a `Cmd msg` what will return a random value, the first argument is the `Msg` the second generates the value
  - `Random.int` - represents a random integer
  - `Random.map` - explains what will happend with that random value. In our case we will use it to take a random element out of a list.

Now lets look at the remaining code. We first define a `board` as a set of the
occupied squares. We use a `Set` because we will have a lot of looking up to do.
Next we to exactly that: we check for every place on the board, if its occupied
(if its in the set) and only filter those out, that are not. Finally we transform
the List into an Array, in order to use the `Array.get` function.

-}
newChicken : List Position -> Cmd Msg
newChicken occupiedSquares =
    let
        emptySquares : Array Position
        emptySquares =
            occupiedSquares
                |> List.map (\pos -> ( pos, () ))
                |> Grid.fromList
                    { columns = boardSize
                    , rows = boardSize
                    }
                |> Grid.emptyPositions
                |> Array.fromList
    in
    Random.generate
        PlaceChicken
        (Random.map
            (\i ->
                emptySquares
                    |> Array.get i
                    |> Maybe.withDefault ( 0, 0 )
            )
            (Random.int 0 ((emptySquares |> Array.length) - 1))
        )


init : () -> ( Model, Cmd Msg )
init _ =
    let
        (( head, body ) as snake) =
            ( ( boardSize // 2, boardSize // 2 ), [] )
    in
    ( { direction = Down
      , snake = snake
      , chicken = Nothing
      }
    , newChicken (head :: body)
    )



{------------------------
   UPDATE
------------------------}


moveSnake : Direction -> Snake -> Snake
moveSnake direction ( pos, body ) =
    let
        dirVec : Coord
        dirVec =
            direction |> Position.fromDirection

        head : Position
        head =
            pos
                |> Position.add dirVec
                |> Tuple.mapBoth (modBy boardSize) (modBy boardSize)
    in
    ( head
    , if body |> List.member head then
        []

      else
        pos :: body
    )


{-| We define the `hungryBody` as the one, that has lost in size, and then we
just say: Unless the snake eats the chicken, it will be hungry
(uses the `hungryBody`).
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Look direction ->
            ( { model | direction = direction }, Cmd.none )

        PlaceChicken pos ->
            ( { model | chicken = Just pos }, Cmd.none )

        Move ->
            let
                ( head, body ) =
                    model.snake |> moveSnake model.direction

                hungryBody : List Position
                hungryBody =
                    body |> List.take ((body |> List.length) - 1)

                defaultCase =
                    ( { model
                        | snake = ( head, hungryBody )
                        , chicken = model.chicken
                      }
                    , Cmd.none
                    )
            in
            case model.chicken of
                Just pos ->
                    if pos == head then
                        -- 🐔👀🐍➡🍗👀🐍
                        ( { model
                            | snake = ( head, body )
                            , chicken = Nothing
                          }
                        , newChicken (head :: body)
                        )

                    else
                        defaultCase

                Nothing ->
                    defaultCase



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
    Time.every (second * 1) (always Move)



{------------------------
   CONTROLS
------------------------}


controls : Input -> Maybe Msg
controls input =
    case input of
        InputUp ->
            Just <| Look Up

        InputDown ->
            Just <| Look Down

        InputLeft ->
            Just <| Look Left

        InputRight ->
            Just <| Look Right

        _ ->
            Nothing



{------------------------
   VIEW
------------------------}


{-|


# Tiles

-}
chickenTile : Tile Msg
chickenTile =
    Tile.fromPosition ( 3, 0 )


snakeHeadTile : Direction -> Tile Msg
snakeHeadTile direction =
    Tile.fromPosition <|
        case direction of
            Down ->
                ( 1, 0 )

            Up ->
                ( 2, 0 )

            Left ->
                ( 1, 1 )

            Right ->
                ( 2, 1 )


snakeBodyTile : Tile Msg
snakeBodyTile =
    Tile.fromPosition ( 3, 1 )


{-|


# Areas

The snake is composed of the head and the body, we can use the `(::)` to connect
the two parts.

-}
viewSnake : Direction -> Snake -> List ( Position, Tile Msg )
viewSnake direction ( ( headX, headY ), body ) =
    ( ( headX + 1, headY + 1 )
    , direction |> snakeHeadTile |> Tile.movable "head"
    )
        :: (body
                |> List.indexedMap
                    (\i ( x, y ) ->
                        ( ( x + 1, y + 1 )
                        , snakeBodyTile |> Tile.movable (String.fromInt i)
                        )
                    )
           )


areas : Model -> List (Area Msg)
areas { snake, direction, chicken } =
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
        (List.concat
            [ snake |> viewSnake direction
            , case chicken of
                Just ( x, y ) ->
                    [ ( ( x + 1, y + 1 ), chickenTile ) ]

                Nothing ->
                    []
            ]
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
    { title = "Snake"
    , options = Just options
    , body = areas model
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
