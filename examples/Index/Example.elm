module Index.Example exposing (view)

import Element exposing (Element)
import Framework.Button as Button
import Framework.Modifier exposing (Modifier(..))
import Framework.Typography as Typography
import Html
import Html.Attributes as Attributes


{-| This is an example
-}
view : String -> Element msg
view name =
    Element.column
        [ Element.width Element.fill
        , Element.height <| Element.px <| 800
        , Element.centerY
        ]
        [ Element.html <|
            Html.iframe
                [ Attributes.src ("examples/"++ name ++ ".html")
                , Attributes.style "height" "100%"
                , Attributes.style "width" "100%"
                , Attributes.style "border-width" "0"
                ]
                []
        , Element.el [ Element.width <| Element.px <| 900,Element.centerX ] <|
            Button.buttonLink
                [ Muted ]
                "#"
                "<| Go Back"
        ]
