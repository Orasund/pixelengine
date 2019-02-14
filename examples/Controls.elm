module ControlsExample exposing (main)

import Color
import PixelEngine exposing (PixelEngine)
import PixelEngine.Controls exposing (Input(..))
import PixelEngine.Graphics as Graphics exposing (Area, Background, Options)
import PixelEngine.Graphics.Image exposing (Image, image)


windowWidth : Int
windowWidth =
    4

tileSize : Int
tileSize =
    16

width : Float
width =
    toFloat <| windowWidth * tileSize


type alias Model =
    { x : Float
    , y : Float
    }


type Msg
    = Controls Input


view : Model -> { title : String, options : Options Msg, body : List (Area Msg) }
view { x, y } =
    let




        background : Background
        background =
            Graphics.colorBackground (Color.rgb255 222 238 214)

        playerTile : Image Msg
        playerTile =
            image "https://orasund.github.io/pixelengine/Controls/cursor.png"
    in
    { title = "Example"
    , options = Graphics.options { width = width, transitionSpeedInSec = 0.2 }
    , body =
        [ Graphics.imageArea
            { height = width
            , background = background
            }
            [ ( ( x, y ) , playerTile)
            ]
        ]
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { x = width / 2, y = width / 2 }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ x, y } as model) =
    ( case msg of
        Controls input ->
            case input of
                InputUp ->
                    if y-1 > -4 then
                        { model | y = y - 1 }
                    else
                        model

                InputLeft ->
                    if x-1 > -4 then
                    { model | x = x - 1 }
                    else
                    model

                InputDown ->
                    if y+1<width - (toFloat tileSize) + 4 then
                    { model | y = y + 1 }
                    else model

                InputRight ->
                    if x+1<width - (toFloat tileSize) + 4 then
                    { model | x = x + 1 }
                    else model
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
