module Controls.Main exposing (main)

import Grid exposing (Grid)
import Grid.Direction exposing (Direction(..))
import Grid.Position as Position exposing (Position)
import PixelEngine
    exposing
        ( Area
        , Input(..)
        , PixelEngine
        , game
        )
import PixelEngine.Options exposing (Options)
import PixelEngine.Tile as Tile exposing (Tile)
import Time



{------------------------
   TYPES
------------------------}


type alias Model =
    { position : ( Int, Int )
    , direction : Direction
    }


{-|


# Actions

What are the things a user should be able to do?

  - We want to be able to place a mark. (`PlaceMark (x,y)`)
  - Once the game is over we want to be able to `reset` the game.

-}
type Msg
    = KeyPressed Direction



{------------------------
   INIT
------------------------}


init : () -> ( Model, Cmd Msg )
init _ =
    ( { position = ( 2, 2 )
      , direction = Down
      }
    , Cmd.none
    )



{------------------------
   UPDATE
------------------------}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ position, direction } as model) =
    case msg of
        KeyPressed dir ->
            ( { model
                | direction = dir
                , position = position |> Position.move 1 dir
              }
            , Cmd.none
            )



{------------------------
   SUBSCRIPTIONS
------------------------}


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



{------------------------
   CONTROLS
------------------------}


controls : Input -> Maybe Msg
controls input =
    case input of
        InputUp ->
            Just <| KeyPressed Up

        InputLeft ->
            Just <| KeyPressed Left

        InputRight ->
            Just <| KeyPressed Right

        InputDown ->
            Just <| KeyPressed Down

        _ ->
            Nothing



{------------------------
   VIEW
------------------------}


directionToPosition : Direction -> Position
directionToPosition dir =
    let
        x : Int
        x =
            case dir of
                Up ->
                    1

                Right ->
                    2

                Down ->
                    3

                Left ->
                    4

        y : Int
        y =
            0
    in
    ( x, y )


tile : Direction -> Tile Msg
tile direction =
    Tile.multipleTiles
        [ Tile.fromPosition ( 0, 0 )
        , Tile.fromPosition <| directionToPosition direction
        ]


tileSize : Int
tileSize =
    16


width : Float
width =
    toFloat <| 5 * tileSize


areas : Model -> List (Area Msg)
areas { position, direction } =
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
        [ ( position, tile direction ) ]
    ]



{------------------------
   CONFIGURATION
------------------------}


view :
    Model
    -> { title : String, options : Maybe (Options Msg), body : List (Area Msg) }
view model =
    { title = "Tic Tac Toe"
    , options = Nothing
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
