module TicTacToe exposing (main)

import Dict exposing (Dict)
import PixelEngine exposing (PixelEngine,game)
import PixelEngine.Controls exposing (Input(..))
import PixelEngine.Graphics as Graphics exposing (Area, Background, Options)
import PixelEngine.Graphics.Tile as Tile exposing (Tile, Tileset, tile)



{------------------------
   TYPES
------------------------}


type Mark
    = Nought
    | Cross


type alias Grid =
    Dict ( Int, Int ) Mark


type alias Model =
    { grid : Grid
    , nextMark : Mark
    }


type Msg
    = SetMark ( Int, Int )
    | Reset
    | None --having a "Do nothing" Msg is often quite a cheap fix.



{------------------------
   INIT
------------------------}


newGame : Model
newGame =
    { grid = Dict.empty
    , nextMark = Cross
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( newGame
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
        SetMark ( x, y ) ->
            if x >= 0 && x < 3 && y >= 0 && y < 3 then
                case grid |> Dict.get ( x, y ) of
                    Nothing ->
                        ( { grid = grid |> Dict.insert ( x, y ) nextMark
                          , nextMark = flip nextMark
                          }
                        , Cmd.none
                        )

                    Just _ ->
                        defaultCase

            else
                {- It is not a good coding practice to have dead branches like
                   this one, but fixing it would blow up this short example.
                   Check out https://www.youtube.com/watch?v=IcgmSRJHu_8 why
                   impossible states are bad.
                -}
                defaultCase

        Reset ->
            ( newGame, Cmd.none )

        None ->
            {- This fires whenever a Keyboard input was made. In our case we
               just ignore it.
            -}
            defaultCase



{------------------------
   SUBSCRIPTIONS
------------------------}


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



{------------------------
   CONTROLS
------------------------}


controls : Input -> Msg
controls _ =
    None



{------------------------
   VIEW
------------------------}


getTile : ( Int, Int ) -> Grid -> Tile Msg
getTile ( x, y ) grid =
    let
        none : ( Int, Int ) -> Tile Msg
        none pos =
            tile ( 0, 0 )
                |> Tile.withAttributes [ Tile.onClick (SetMark pos) ]

        nought : Tile Msg
        nought =
            tile ( 0, 1 )

        cross : Tile Msg
        cross =
            tile ( 1, 1 )
    in
    case grid |> Dict.get ( x, y ) of
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

        reset : Tile Msg
        reset =
            tile ( 1, 0 ) |> Tile.withAttributes [ Tile.onClick Reset ]
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
                { width = 2, height = 2 }
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
    game
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , controls = controls
        }
