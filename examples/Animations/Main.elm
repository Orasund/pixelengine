module Animations exposing (main)

import Color
import Html.Attributes as Attributes
import PixelEngine exposing (gameWithNoControls)
import PixelEngine.Graphics as Graphics exposing (Background)
import PixelEngine.Graphics.Image exposing (fromTile, image, multipleImages, withAttributes)
import PixelEngine.Graphics.Tile as Tile
    exposing
        ( Tileset
        , tile
        )


type alias Model =
    ()


type alias Msg =
    Never


view () =
    let
        tileSize : Int
        tileSize =
            16

        windowWidth : Int
        windowWidth =
            16

        width : Float
        width =
            toFloat <| 2 * tileSize

        tileset : Tileset
        tileset =
            { source = "https://orasund.github.io/pixelengine/Animations/tileset.png"
            , spriteWidth = 16, spriteHeight = 16 }

        background : Background
        background =
            Graphics.colorBackground (Color.rgb255 222 238 214)
    in
    { title = "Example for Animations"
    , options = Graphics.options { width = width, transitionSpeedInSec = 0.2 }
    , body =
        [ Graphics.imageArea
            { height = width
            , background = background
            }
            [ ( ( 8, 8 )
              , fromTile (tile ( 0, 0 ) |> Tile.animated 3) tileset
                {- |> withAttributes [ Attributes.style "float" "left"] -}
              )
            ]
        ]
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( (), Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update _ m =
    ( m, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions m =
    Sub.none


main =
    gameWithNoControls
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
