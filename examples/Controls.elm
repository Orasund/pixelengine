module ControlsExample exposing (main)

import Color
import PixelEngine exposing (PixelEngine)
import PixelEngine.Controls exposing (Input(..))
import PixelEngine.Graphics as Graphics exposing (Area, Background, Options)
import PixelEngine.Graphics.Tile exposing (Tile, Tileset, tile)


windowWidth : Int
windowWidth =
    16


type alias Model =
    { x : Int
    , y : Int
    }


type Msg
    = Controls Input


view : Model -> { title : String, options : Options Msg, body : List (Area Msg) }
view { x, y } =
    let
        tileSize : Int
        tileSize =
            16

        width : Float
        width =
            toFloat <| windowWidth * tileSize

        tileset : Tileset
        tileset =
            { source = "tileset.png", spriteWidth = 16, spriteHeight = 16 }

        background : Background
        background =
            Graphics.colorBackground (Color.rgb255 20 12 28)

        playerTile : Tile Msg
        playerTile =
            tile ( 12, 12 )
    in
    { title = "Example"
    , options = Graphics.options { width = width, transitionSpeedInSec = 0.2 }
    , body =
        [ Graphics.tiledArea
            { rows = windowWidth
            , background = background
            , tileset = tileset
            }
            [ ( ( x, y )
              , playerTile
              )
            ]
        ]
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { x = windowWidth // 2, y = windowWidth // 2 }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ x, y } as model) =
    ( case msg of
        Controls input ->
            case input of
                InputUp ->
                    { model | y = y - 1 }

                InputLeft ->
                    { model | x = x - 1 }

                InputDown ->
                    { model | y = y + 1 }

                InputRight ->
                    { model | x = x + 1 }

                _ ->
                    model
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


controls : Input -> Msg
controls =
    Controls


main : PixelEngine () Model Msg
main =
    PixelEngine.game
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , controls = controls
        }
