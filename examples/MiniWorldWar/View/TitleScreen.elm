module MiniWorldWar.View.TitleScreen exposing (normal, waiting)

import MiniWorldWar.View exposing (tileSize)
import PixelEngine.Graphics.Image as Image exposing (Image, image)


logoImage : ( ( Float, Float ), Image msg )
logoImage =
    ( ( 0, tileSize * 1 ), image "logo.png" )


waiting : List ( ( Float, Float ), Image msg )
waiting =
    [ logoImage
    , ( ( tileSize * 2, tileSize * 5 ), image "findingOpponent.png" )
    ]


normal : msg -> List ( ( Float, Float ), Image msg )
normal msg =
    [ logoImage
    , ( ( tileSize * 2, tileSize * 5 )
      , image "newGame.png"
            |> Image.withAttributes
                [ Image.onClick msg ]
      )
    ]
