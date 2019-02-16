module Snake exposing (main)

import Array exposing (Array)
import Dict exposing (Dict)
import PixelEngine exposing (PixelEngine, game)
import PixelEngine.Controls exposing (Input(..))
import PixelEngine.Graphics as Graphics exposing (Area, Background, Options)
import PixelEngine.Graphics.Tile as Tile exposing (Tile, Tileset, tile)
import PixelEngine.Grid.Direction as Direction exposing (Direction(..))
import PixelEngine.Grid.Position as Position exposing (Position,Vector)
import Random
import Set exposing (Set)
import Time



{------------------------
   TYPES
------------------------}


type alias Snake =
    ( Position, List Position )


type alias Model =
    { direction : Direction
    , snake : Snake
    , chicken : Maybe Position
    }


type Msg
    = Look Direction
    | PlaceChicken Position
    | Move
    | None



{------------------------
   GLOBAL VARIABLES
------------------------}


boardSize : Int
boardSize =
    4



{------------------------
   INIT
------------------------}


newChicken : List Position -> Cmd Msg
newChicken occupiedSquares =
    let
        board : Set Position
        board =
            Set.fromList occupiedSquares

        emptySquares : Array Position
        emptySquares =
            List.range 0 (boardSize - 1)
                |> List.map
                    (\x ->
                        List.range 0 (boardSize - 1)
                            |> List.map (\y -> ( x, y ))
                    )
                |> List.concat
                |> List.filter (\pos -> not <| Set.member pos board)
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
        dirVec : Vector
        dirVec =
            direction |> Position.fromDirection 

        head : Position
        head = 
            pos |> Position.add dirVec
                |> Tuple.mapBoth (modBy boardSize) (modBy boardSize)

    in
    ( head
    , if body |> List.member head then
        []

      else
        pos :: body
    )


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

        None ->
            ( model, Cmd.none )



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
   Controls
------------------------}


controls : Input -> Msg
controls input =
    case input of
        InputUp ->
            Look Up

        InputDown ->
            Look Down

        InputLeft ->
            Look Left

        InputRight ->
            Look Right

        _ ->
            None

{------------------------
   VIEW
------------------------}


emptyTile : Tile Msg
emptyTile =
    tile ( 0, 1 )


chickenTile : Tile Msg
chickenTile =
    tile ( 3, 0 )


snakeHeadTile : Direction -> Tile Msg
snakeHeadTile direction =
    tile <|
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
    tile ( 3, 1 )


viewSnake : Direction -> Snake -> List ( Position, Tile Msg )
viewSnake direction ( ( headX, headY ), body ) =
    ( ( headX + 1, headY + 1 )
    , direction |> snakeHeadTile
    )
        :: (body |> List.map (\( x, y ) -> ( ( x + 1, y + 1 ), snakeBodyTile )))


view : Model -> { title : String, options : Options Msg, body : List (Area Msg) }
view { snake, direction, chicken } =
    let
        tileSize : Int
        tileSize =
            32

        width : Float
        width =
            toFloat <| (boardSize + 2) * tileSize

        tileset : Tileset
        tileset =
            { source = "tileset.png"
            , spriteWidth = tileSize
            , spriteHeight = tileSize
            }

        background : Background
        background =
            Graphics.imageBackground
                { height = width
                , width = width
                , source = "background.png"
                }
    in
    { title = "Snake"
    , options = Graphics.options { width = width, transitionSpeedInSec = 0.2 }
    , body =
        [ Graphics.tiledArea
            { rows = boardSize + 2
            , tileset = tileset
            , background = background
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
    }







{------------------------
   MAIN
------------------------}


main : PixelEngine () Model Msg
main =
    game
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , controls = controls
        }
