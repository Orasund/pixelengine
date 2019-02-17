module RuinJump.MapSegment exposing (append, concat, floorGenerator, intersectionGenerator, parkourGenerator)

import CellAutomata exposing (Automata, Rule, RuleExpression(..))
import Dict
import Natural exposing (Natural16(..))
import Random exposing (Generator)
import RuinJump.Automata as Automata exposing (Grid, automata, mirroringAutomata)
import RuinJump.Config as Config
import RuinJump.Map exposing (Map)
import RuinJump.MapElement exposing (Block(..), MapElement(..))
import RuinJump.Rules as Rules
import PixelEngine.Grid.Position exposing(Position)

width : Int
width =
    Config.width


height : Int
height =
    Config.sectionHeight


append : Generator Map -> Generator Map -> Generator Map
append =
    Random.map2
        (\map segment ->
            map |> Dict.union segment
        )


concat : List (Generator Map) -> Generator Map
concat list =
    case list of
        [] ->
            Random.constant Dict.empty

        a :: tail ->
            tail
                |> List.foldl
                    append
                    a


newSeed : Int -> Position -> Random.Seed
newSeed seed ( x, y ) =
    Random.initialSeed (seed + x * width + y)


toSegment : Int -> Grid -> Map
toSegment seed =
    Dict.map
        (\pos block ->
            BlockElement block <|
                Tuple.first <|
                    Random.step (Random.int 0 Random.maxInt) <|
                        newSeed seed <|
                            pos
        )


stepWithAutomata : (List (Rule Block) -> Automata Block) -> Int -> List (Rule Block) -> Grid -> Grid
stepWithAutomata customAutomata yOffset rules grid =
    List.range 0 width
        |> List.foldl
            (\x partList ->
                List.range (-1 * height - yOffset) -yOffset
                    |> List.foldl
                        (\y list ->
                            let
                                pos : Position
                                pos =
                                    ( x, y )

                                elem : Maybe Block
                                elem =
                                    grid |> Dict.get pos
                            in
                            case Automata.step (customAutomata rules) grid pos elem of
                                Just a ->
                                    ( pos, a ) :: list

                                Nothing ->
                                    list
                        )
                        partList
            )
            []
        |> Dict.fromList


step : Int -> List (Rule Block) -> Grid -> Grid
step =
    stepWithAutomata automata


mirroringStep : Int -> List (Rule Block) -> Grid -> Grid
mirroringStep =
    stepWithAutomata mirroringAutomata


repeat : Int -> (a -> a) -> a -> a
repeat num fun dict =
    List.range 0 (num - 1)
        |> List.foldl (always <| fun) dict


walls : Int -> List ( Position, Block )
walls yOffset =
    List.concat
        [ List.repeat height Stone
            |> List.indexedMap (\x elem -> ( ( 0, -x - yOffset ), elem ))
        , List.repeat height Stone
            |> List.indexedMap (\x elem -> ( ( width - 1, -x - yOffset ), elem ))
        , [ ( ( 1, -yOffset ), Stone ), ( ( width - 2, -yOffset ), Stone ) ]
        ]


horizontalLine : Int -> List ( Position, Block )
horizontalLine y =
    List.concat
        [ List.repeat (width // 4) Stone
            |> List.indexedMap (\x elem -> ( ( 1 + x * 4, y ), elem ))
        , List.repeat (width // 4) Stone
            |> List.indexedMap (\x elem -> ( ( 2 + x * 4, y ), elem ))
        ]

sparceHorLine : Int -> List ( Position, Block )
sparceHorLine y =
    List.concat
        [ List.repeat (width // 8) Stone
            |> List.indexedMap (\x elem -> ( ( 3 + x * 8, y ), elem ))
        , List.repeat (width // 8) Stone
            |> List.indexedMap (\x elem -> ( ( 4 + x * 8, y ), elem ))
        ]

piliar : Position -> Int -> List ( Position, Block )
piliar ( x, y ) yOffset =
    [ ( ( x, -y - yOffset ), Stone )
    , ( ( x + 1, -y - yOffset ), Stone )
    , ( ( x + 2, -y - yOffset ), Stone )
    , ( ( x, -y + 1 - yOffset ), Stone )
    , ( ( x + 2, -y + 1 - yOffset ), Stone )
    ]


intersectionGenerator : Int -> Generator Map
intersectionGenerator level =
    let
        yOffset : Int
        yOffset =
            level * height

        segment : Grid
        segment =
            [ walls yOffset
            , List.repeat (height // 4) Stone
                |> List.indexedMap (\y elem -> ( ( 1, -y * 4 - yOffset ), elem ))
            , List.repeat (height // 4) Stone
                |> List.indexedMap (\y elem -> ( ( width - 2, -y * 4 - yOffset ), elem ))
            , horizontalLine <| -yOffset
            , sparceHorLine <| -3 - yOffset
            , sparceHorLine <| -6 - yOffset
            , piliar (width//2+1,1) yOffset
            , piliar (width//2-4,1) yOffset
            , piliar (width//2+1,3) yOffset
            , piliar (width//2-4,3) yOffset
            , piliar (width//2+1,5) yOffset
            , piliar (width//2-4,5) yOffset
            , piliar (width//2+1,7) yOffset
            , piliar (width//2-4,7) yOffset
            ]
                |> List.concat
                |> Dict.fromList
    in
    Random.map
        (\seed ->
            segment |> toSegment seed
        )
        (Random.int Random.minInt Random.maxInt)


parkourGenerator : Int -> Generator Map
parkourGenerator level =
    let
        yOffset : Int
        yOffset =
            level * height

        build : List (Maybe Block) -> List ( Position, Block )
        build =
            List.indexedMap (\x -> Maybe.map (\elem -> ( ( x |> modBy width, -1 - yOffset - 2 * (x // width) ), elem )))
                >> List.filterMap identity
    in
    Random.map2
        (\seed ->
            build
                >> Dict.fromList
                >> step yOffset Rules.parkour
                >> Dict.union (walls yOffset |> Dict.fromList)
                >> repeat 2 (step yOffset Rules.placeDirt)
                >> repeat 2 (step yOffset Rules.placeGrass)
                >> repeat 3 (mirroringStep yOffset Rules.removeGrass)
                >> toSegment seed
        )
        (Random.int Random.minInt Random.maxInt)
        (Random.list (width * (height // 2)) <| Random.weighted ( 2 + toFloat level, Nothing ) [ ( 1 + (toFloat <| level // 4), Just Stone ), ( 4, Just Dirt ) ])


floorGenerator : Int -> Generator Map
floorGenerator level =
    let
        yOffset : Int
        yOffset =
            level * height

        build : List (Maybe Block) -> List ( Position, Block )
        build =
            List.indexedMap (\x -> Maybe.map (\elem -> ( ( x, -2 - yOffset ), elem )))
                >> List.filterMap identity
    in
    Random.map2
        (\seed ->
            build
                >> (\list ->
                        List.concat
                            [ list
                            , List.repeat width Dirt
                                |> List.indexedMap (\x elem -> ( ( x, -1 - yOffset ), elem ))
                            , List.repeat width Dirt
                                |> List.indexedMap (\x elem -> ( ( x, 0 - yOffset ), elem ))
                            , piliar ( width - 1 - 5 - 2, height - 1 ) yOffset
                            , piliar ( 5, height - 1 ) yOffset
                            ]
                   )
                >> Dict.fromList
                >> repeat 2 (step yOffset Rules.placeDirt)
                >> repeat 2 (step yOffset Rules.placeGrass)
                >> toSegment seed
        )
        (Random.int Random.minInt Random.maxInt)
        (Random.list width (Random.weighted ( 3, Nothing ) [ ( 10, Just Dirt ) ]))
