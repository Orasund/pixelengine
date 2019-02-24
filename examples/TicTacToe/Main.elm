module TicTacToe exposing (main)

import Grid exposing (Grid)
import Grid.Position exposing (Position)
import PixelEngine exposing (Area, Background, Input(..), PixelEngine, gameWithNoControls)
import PixelEngine.Options as Options exposing (Options)
import PixelEngine.Tile as Tile exposing (Tile, Tileset)



{------------------------
   TYPES
------------------------}


{-|


# Players

In Tic Tac Toe we have two players: one is playing _Noughts_, the other _Crosses_.

![Noughts and Crosses](https://orasund.github.io/pixelengine/docs/tictactoe1.png "Noughts and Crosses")

-}
type Mark
    = Nought
    | Cross


{-|


# State

The state of an ongoing Tic Tac Toe-game consists of two things:
The board(`grid`) and the current player(`nextMark`).

![Model](https://orasund.github.io/pixelengine/docs/tictactoe2.png "Model")

For the grid we use a custom type called Grid, you can think of it as a
`Dict (Int,Int) Mark` with a fixed size.

**Note:**
Think of the `Model` as the state of the game. In Elm the convention is to name
it `Model`, but for games this term might be confusing.

-}
type alias Model =
    { grid : Grid Mark
    , nextMark : Mark
    }


{-|


# Actions

What are the things a user should be able to do?

  - We want to be able to place a mark. (`PlaceMark (x,y)`)
  - Once the game is over we want to be able to `reset` the game.

-}
type Msg
    = PlaceMark Position
    | Reset



{------------------------
   INIT
------------------------}


init : () -> ( Model, Cmd Msg )
init _ =
    ( { grid =
            Grid.empty
                { columns = 3
                , rows = 3
                }
      , nextMark = Cross
      }
    , Cmd.none
    )



{------------------------
   UPDATE
------------------------}


flip : Mark -> Mark
flip mark =
    case mark of
        Nought ->
            Cross

        Cross ->
            Nought


{-| The update function is very straight forward: First we validate if this move
is actually legit, and then we update the board accordently and flip the current
player.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ grid, nextMark } as model) =
    let
        defaultCase : ( Model, Cmd Msg )
        defaultCase =
            ( model, Cmd.none )
    in
    case msg of
        PlaceMark pos ->
            case grid |> Grid.get pos of
                Nothing ->
                    ( { grid = grid |> Grid.insert pos nextMark
                      , nextMark = flip nextMark
                      }
                    , Cmd.none
                    )

                Just _ ->
                    defaultCase

        Reset ->
            init ()



{------------------------
   SUBSCRIPTIONS
------------------------}


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



{------------------------
   VIEW
------------------------}


{-|


# Tiles

For our game we use four Tiles:

  - Empty Cell
  - Cell filled with a nought
  - Cell filled with a cross
  - Restart button

![Tileset](https://orasund.github.io/pixelengine/docs/tictactoe3.png "Tileset")

**Note:**
Everything that is clickable needs to be a `Tile`. That's why the restart button
is a `Tile` as well.

-}
none : Position -> Tile Msg
none pos =
    Tile.fromPosition ( 0, 0 )
        |> Tile.clickable (PlaceMark pos)


nought : Tile Msg
nought =
    Tile.fromPosition ( 0, 1 )


cross : Tile Msg
cross =
    Tile.fromPosition ( 1, 1 )


reset : Tile Msg
reset =
    Tile.fromPosition ( 1, 0 ) |> Tile.clickable Reset


getTile : Position -> Maybe Mark -> Tile Msg
getTile ( x, y ) mark =
    case mark of
        Just Nought ->
            nought

        Just Cross ->
            cross

        Nothing ->
            none ( x, y )


{-|


# Grid

The `Grid` datatype takes care of iteration over all elements of the grid.

-}
viewGrid : Grid Mark -> List ( Position, Tile Msg )
viewGrid grid =
    grid
        |> Grid.foldl
            (\(( x, y ) as pos) maybeMark ->
                (::)
                    ( ( 1 + x, 1 + y )
                    , maybeMark |> getTile pos
                    )
            )
            []


tileSize : Int
tileSize =
    16


width : Float
width =
    toFloat <| 5 * tileSize


{-|


# Areas

-}
areas : Model -> List (Area Msg)
areas { grid } =
    [ PixelEngine.tiledArea
        { rows = 5
        , tileset =
            { source = "tileset.png"
            , spriteWidth = tileSize
            , spriteHeight = tileSize
            }
        , background =
            PixelEngine.imageBackground
                { height = 80
                , width = 80
                , source = "background.png"
                }
        }
        (List.concat
            [ grid |> viewGrid
            , [ ( ( 2, 0 ), reset ) ]
            ]
        )
    ]



{------------------------
   CONFIGURATION
------------------------}


view : Model -> { title : String, options : Maybe (Options Msg), body : List (Area Msg) }
view model =
    { title = "Tic Tac Toe"
    , options = Nothing
    , body = areas model
    }


main : PixelEngine () Model Msg
main =
    gameWithNoControls
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , width = width
        }
