module MiniWorldWar.View.Error exposing (view)

import Http exposing (Error(..))
import PixelEngine.Image as Image exposing (Image)

view : Error -> List ( ( Float, Float ), Image msg )
view error =
    [ ( ( 0, 0 )
      , { source = "Berlin8x8.png"
        , spriteWidth = 8
        , spriteHeight = 8
        }
            |> (case error of
                    BadUrl string ->
                        Image.fromText <| "badUrl:" ++ string

                    Timeout ->
                        Image.fromText "timeout"

                    NetworkError ->
                        Image.fromText "networkError"

                    BadStatus int ->
                        Image.fromText <| "badStatus:" ++ (String.fromInt <| int)

                    BadBody string ->
                        Image.fromText <| ("badBody:" ++ string)
               )
      )
    ]
