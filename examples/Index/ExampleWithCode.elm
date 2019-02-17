module Index.ExampleWithCode exposing (view)

import Element exposing (Element)
import Element.Background as Background
import Framework.Card as Card
import Framework.Typography as Typography
import Html
import Html.Attributes as Attributes
import Index.Example as Example
import Markdown.Block as Markdown exposing (Block(..))
import Parser exposing ((|.), (|=), Parser, Step(..))
import Result
import Set
import SyntaxHighlight


type ParsedType
    = Code String
    | Comment String


markdown : Block b i -> Element msg
markdown block =
    let
        defaultCase : Element msg
        defaultCase =
            Element.html <|
                Html.div [] <|
                    Markdown.toHtml block
    in
    case block of
        _ ->
            defaultCase


codeParser : Parser ParsedType
codeParser =
    Parser.succeed ()
        |. Parser.chompUntilEndOr "\u{000D}\n\u{000D}\n{-|"
        |> Parser.getChompedString
        |> Parser.map Code


commentParser : Parser ParsedType
commentParser =
    Parser.succeed Comment
        |. Parser.keyword "\u{000D}\n\u{000D}\n{-|"
        |= (Parser.chompUntil "-}" |> Parser.getChompedString)
        |. Parser.keyword "-}\u{000D}"
        |. Parser.chompWhile (\char -> char == '\n' || char == '\u{000D}' || char == '\t')


multipleHelp : List ParsedType -> Parser (Step (List ParsedType) (List ParsedType))
multipleHelp list =
    Parser.oneOf
        [ Parser.succeed (Done <| Debug.log "DoneEnd" <| list)
            |. Parser.end
        , Parser.succeed (\e -> Loop (Debug.log "LoopComment" <| List.append list [ e ]))
            |= commentParser
        , Parser.succeed (\e -> Loop (Debug.log "LoopCode" <| List.append list [ e ]))
            |= codeParser
        , Parser.succeed (Done <| Debug.log "Done" <| list)
        ]


parseMultiple : Parser (List ParsedType)
parseMultiple =

    Parser.succeed identity
        |. Parser.multiComment "module" ")\r" Parser.NotNestable
        |. Parser.keyword ")"
        |= Parser.loop [] multipleHelp


parse : String -> List (Element msg)
parse =
    Parser.run parseMultiple
        >> Result.withDefault []
        >> List.map
            (\elem ->
                case elem of
                    Code code ->
                        Element.el
                            [ Element.width Element.fill
                            , Element.paddingXY 20 0
                            , Background.color <| Element.rgb255 255 255 255
                            ]
                        <|
                            Element.html <|
                                Html.div []
                                    [ SyntaxHighlight.useTheme SyntaxHighlight.gitHub
                                    , SyntaxHighlight.elm code
                                        |> Result.map (SyntaxHighlight.toBlockHtml Nothing)
                                        |> Result.withDefault
                                            (Html.code [] [ Html.text code ])
                                    ]

                    Comment string ->
                        Element.column [] <|
                            List.map markdown <|
                                Markdown.parse Nothing string
            )


view : { src : String, code : String } -> Element msg
view { src, code } =
    Element.column [ Element.centerX, Element.width Element.fill ]
        [ Example.view src
        , Element.column
            [ Element.width <| Element.px 800
            , Element.centerX
            , Element.padding 20
            , Element.spacing 10
            ]
            (parse code)
        ]
