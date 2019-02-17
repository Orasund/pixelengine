module TicTacToe exposing (main)

import PixelEngine exposing (PixelEngine, gameWithNoControls)
import PixelEngine.Controls exposing (Input(..))
import PixelEngine.Graphics as Graphics exposing (Area, Background, Options)
import PixelEngine.Graphics.Tile as Tile exposing (Tile, Tileset, tile)
import PixelEngine.Grid.Position exposing (Position)
import PixelEngine.Grid as Grid exposing (Grid)



{------------------------
   TYPES
------------------------}


type Mark
    = Nought
    | Cross


type alias Model =
    { grid : Grid Mark
    , nextMark : Mark
    }


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


none : ( Int, Int ) -> Tile Msg
none pos =
    tile ( 0, 0 )
        |> Tile.withAttributes [ Tile.onClick (PlaceMark pos) ]


nought : Tile Msg
nought =
    tile ( 0, 1 )


cross : Tile Msg
cross =
    tile ( 1, 1 )


reset : Tile Msg
reset =
    tile ( 1, 0 ) |> Tile.withAttributes [ Tile.onClick Reset ]


getTile : ( Int, Int ) -> Grid Mark -> Tile Msg
getTile ( x, y ) grid =
    case grid |> Grid.get ( x, y ) of
        Just Nought ->
            nought

        Just Cross ->
            cross

        Nothing ->
            none ( x, y )


foldOverGrid : { width : Int, height : Int } -> (( Int, Int ) -> a) -> List a
foldOverGrid { width, height } function =
    List.range 0 (width - 1)
        |> List.foldl
            (\x list ->
                List.range 0 (height - 1)
                    |> List.foldl
                        (\y -> List.append [ function ( x, y ) ])
                        list
            )
            []


view : Model -> { title : String, options : Options Msg, body : List (Area Msg) }
view { grid } =
    let
        tileSize : Int
        tileSize =
            16

        windowWidth : Int
        windowWidth =
            5

        width : Float
        width =
            toFloat <| windowWidth * tileSize

        tileset : Tileset
        tileset =
            { source = "tileset.png"
            , spriteWidth = tileSize
            , spriteHeight = tileSize
            }

        background : Background
        background =
            Graphics.imageBackground
                { height = 80
                , width = 80
                , source = "background.png"
                }
    in
    { title = "Tic Tac Toe"
    , options = Graphics.options { width = width, transitionSpeedInSec = 0.2 }
    , body =
        [ Graphics.tiledArea
            { rows = 5
            , tileset = tileset
            , background = background
            }
            (foldOverGrid
                { width = 3, height = 3 }
                (\(( x, y ) as pos) ->
                    ( ( 1 + x, 1 + y )
                    , grid |> getTile pos
                    )
                )
                |> List.append
                    [ ( ( 2, 0 ), reset ) ]
            )
        ]
    }



{------------------------
   MAIN
------------------------}


main : PixelEngine () Model Msg
main =
    gameWithNoControls
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
