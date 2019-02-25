module MiniWorldWar.View.TitleScreen exposing (normal, waiting)

import MiniWorldWar.View exposing (tileSize)
import PixelEngine.Image as Image exposing (Image)


logoImage : ( ( Float, Float ), Image msg )
logoImage =
    ( ( 0, tileSize * 1 ), Image.fromSrc "logo.png" )


waiting : List ( ( Float, Float ), Image msg )
waiting =
    [ logoImage
    , ( ( tileSize * 2, tileSize * 5 ), Image.fromSrc "findingOpponent.png" )
    ]


normal : msg -> List ( ( Float, Float ), Image msg )
normal msg =
    [ logoImage
    , ( ( tileSize * 2, tileSize * 5 )
      , Image.fromSrc "newGame.png"
            |> Image.clickable msg
      )
    ]
