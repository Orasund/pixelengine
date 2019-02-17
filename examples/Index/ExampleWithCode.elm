module Index.ExampleWithCode exposing (view)

import Element exposing (Element)
import Element.Background as Background
import Element.Font as Font
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
    | Section String


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

sectionParser : Parser ParsedType
sectionParser =
    Parser.succeed Section
        |. Parser.token "\n\n{--"
        |. Parser.chompWhile ((==) '-')
        |= Parser.variable
            { start = always True
            , inner = ((/=) '-')
            , reserved= Set.empty
            }
        |. Parser.chompWhile ((==) '-')
        |. Parser.token "}"

codeParser : Parser ParsedType
codeParser =
    Parser.succeed ()
        |. Parser.chompWhile (\char -> char == '\n' || char == '\u{000D}' || char == '\t')
        |. Parser.chompUntilEndOr "\n\n{-"
        |> Parser.getChompedString
        |> Parser.map Code


commentParser : Parser ParsedType
commentParser =
    Parser.succeed Comment
        |. Parser.token "\n\n{-|"
        |= (Parser.chompUntil "-}" |> Parser.getChompedString)
        |. Parser.token "-}\n"
        |. Parser.chompWhile (\char -> char == '\n' || char == '\u{000D}' || char == '\t')


multipleHelp : List ParsedType -> Parser (Step (List ParsedType) (List ParsedType))
multipleHelp list =
    Parser.oneOf
        [ Parser.succeed (Done list)
            |. Parser.end
        , Parser.succeed (\e -> Loop (List.append list [ e ]))
            |= sectionParser 
        , Parser.succeed (\e -> Loop (List.append list [ e ]))
            |= commentParser
                               
        , Parser.succeed (\e -> Loop (List.append list [ e ]))
            |= codeParser
            
        , Parser.succeed (Done <| list)
        ]


parseMultiple : Parser (List ParsedType)
parseMultiple =
    Parser.succeed identity
        |. Parser.multiComment "module" ")\n" Parser.NotNestable
        |. Parser.token ")"
        |= Parser.loop [] multipleHelp


parse : String -> List (Element msg)
parse string =
    let
        displayCode : String -> Element msg
        displayCode code =
            Element.el
                [ Element.width Element.fill
                , Element.paddingXY 20 0
                , Background.color <| Element.rgb255 255 255 255
                , Font.size <| 16
                ]
            <|
                Element.html <|
                    Html.div [ Attributes.style "line-height" "1.2" ]
                        [ SyntaxHighlight.useTheme SyntaxHighlight.gitHub
                        , SyntaxHighlight.elm code
                            |> Result.map (SyntaxHighlight.toBlockHtml Nothing)
                            |> Result.withDefault
                                (Html.code [] [ Html.text code ])
                        ]
    in
    case
        string
            |> Parser.run parseMultiple
            |> Result.withDefault []
    of
        [] ->
            [ string |> displayCode ]

        list ->
            list
                |> List.map
                    (\elem ->
                        case elem of
                            Code code ->
                                code |> displayCode

                            Comment comment ->
                                Element.paragraph [] <|
                                    List.map markdown <|
                                        Markdown.parse Nothing comment
                            
                            Section title ->
                                Typography.h1 [] <|
                                    Element.text title
                    )


view : { src : String, code : String } -> Element msg
view { src, code } =
    Element.column [ Element.centerX, Element.width Element.fill ]
        [ Example.view src
        , Element.column
            [ Element.width <| Element.px 900
            , Element.centerX
            , Element.padding 20
            , Element.spacing 10
            ]
            (parse (code |> Debug.log "Gotit:"))
        ]
