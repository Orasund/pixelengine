module PixelEngine.Graphics.Abstract exposing
    ( Compatible(..)
    , TilesetImage
    , render
    )

import Css exposing (px)
import Css.Global as Global
import Html.Styled as Html exposing (Html, div)
import Html.Styled.Attributes as Attributes exposing (css)
import PixelEngine.Graphics.Data exposing (Background(..))
import PixelEngine.Graphics.Data.Area exposing (Area(..))
import PixelEngine.Graphics.Data.Controller as Controller
import PixelEngine.Graphics.Data.Element exposing (Element(..), SingleSource(..))
import PixelEngine.Graphics.Data.Options exposing (Options(..))
import PixelEngine.Graphics.Data.Tile exposing (Tile)
import PixelEngine.Graphics.Data.Transition exposing (Transition(..))
import PixelEngine.Graphics.View.Area as AreaView


type alias TilesetImage msg =
    { source : String
    , tile : Tile msg
    }


type Compatible
    = Compatible


render : Options msg -> List (Area msg) -> Html msg
render ((Options { width, scale, transitionFrom, transition, controllerOptions }) as options) to =
    let
        (Transition { name, transitionList }) =
            transition

        transitionLength : Float
        transitionLength =
            transitionList
                |> List.map Tuple.first
                |> List.sum

        animationCss =
            transitionList
                |> List.foldl
                    (\( length, cssCommands ) ( sum, string ) ->
                        ( sum + length
                        , string
                            ++ (String.fromFloat <| (sum + length) * 100 / transitionLength)
                            ++ "% {"
                            ++ "visibility:visible;"
                            ++ cssCommands
                            ++ "} "
                        )
                    )
                    ( 0, "" )
                |> Tuple.second
                |> (\a -> a ++ ";")
    in
    div
        [ css
            [ Css.backgroundColor (Css.rgb 0 0 0)
            ]
        ]
        ([ Global.global
            [ Global.selector
                ("@keyframes pixelengine_screen_transition_"
                    ++ name
                )
                [ Css.property
                    animationCss
                    ""
                ]
            ]
         , Html.node "meta"
            [ Attributes.name "viewport"
            , Attributes.attribute "content" "width=device-width, initial-scale=1, maximum-scale=1, user-scalable=no, user-scalable=0"
            ]
            []
         , Html.node "meta"
            [ Attributes.name "apple-mobile-web-app-capable"
            , Attributes.attribute "content" "yes"
            ]
            []
         , Html.node "meta"
            [ Attributes.name "apple-mobile-web-app-status-bar-style"
            , Attributes.attribute "content" "black-translucent"
            ]
            []
         , div
            [ css
                [ Css.position Css.relative
                , Css.width <| px <| (toFloat <| scale) * width
                , Css.margin Css.auto
                ]
            ]
            [ div
                []
                [ AreaView.view options to ]
            , div
                [ css
                    [ Css.position Css.absolute
                    , Css.top <| px 0
                    , Css.left <| px 0
                    , Css.visibility Css.hidden
                    , Css.property "animation"
                        ("pixelengine_screen_transition_"
                            ++ name
                            ++ " "
                            ++ String.fromFloat transitionLength
                            ++ "s 1"
                        )
                    ]
                ]
                [ AreaView.view options transitionFrom ]
            ]
         ]
            |> (case controllerOptions of
                    Nothing ->
                        identity

                    Just ({ windowSize } as justCtrlOptions) ->
                        if windowSize.width > 1.5 * windowSize.height then
                            \l -> Controller.render justCtrlOptions |> List.append l

                        else
                            identity
               )
        )
