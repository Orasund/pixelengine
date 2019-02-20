module Index.Example exposing (view)

import Element exposing (Element)
import Framework.Button as Button
import Framework.Modifier exposing (Modifier(..))
import Html
import Html.Attributes as Attributes


view : String -> Element msg
view name =
    Element.column
        [ Element.width Element.fill
        , Element.height <| Element.px <| (512 + 28)
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
            , Button.buttonLink
                []
                ("https://github.com/Orasund/pixelengine/blob/master/examples/" ++ name ++ "/Main.elm")
                "view on Github |>"
            ]
        ]
