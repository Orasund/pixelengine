module Index.Example exposing (view)

import Element exposing (Element)
import Framework.Button as Button
import Framework.Color as Color
import Framework.Icon as Icon
import Framework.Modifier exposing (Modifier(..))
import Html
import Html.Attributes as Attributes


view : String -> Element msg
view name =
    Element.column
        [ Element.width Element.fill
        , Element.height <| Element.px <| (512 + 44)
        , Element.centerY
        ]
        [ Element.html <|
            Html.iframe
                [ Attributes.src ("examples/" ++ name ++ "/index.html")
                , Attributes.style "height" "100%"
                , Attributes.style "width" "100%"
                , Attributes.style "border-width" "0"
                ]
                []
        , Element.row [ Element.width <| Element.px <| 900, Element.centerX, Element.spaceEvenly ]
            [ Button.buttonLink
                [ Muted ]
                "#"
                "<| Go Back"

            {- , Element.link []
               { url = "https://orasund.github.io/pixelengine/examples/" ++ name ++ "/index.html"
               , label =
                   Icon.fullscreen
                       Color.white
                       16
               }
            -}
            , Button.buttonLink
                []
                ("https://github.com/Orasund/pixelengine/blob/master/examples/" ++ name ++ "/Main.elm")
                "view on Github |>"
            ]
        ]
