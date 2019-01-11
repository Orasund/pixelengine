module RuinJump.Automata exposing (Grid, automata, mirroringAutomata, order, step)

import CellAutomata as Automata exposing (Automata, Location, Rule)
import RuinJump.MapElement exposing (Block(..))


type alias Grid =
    Automata.Grid Block


order : Maybe Block -> Int
order maybeBlock =
    case maybeBlock of
        Just Air ->
            0

        Nothing ->
            0

        Just Dirt ->
            1

        Just Grass ->
            2

        Just Stone ->
            3

        Just Wood ->
            4


automata : List (Rule Block) -> Automata Block
automata rules =
    Automata.automataWithoutSymmetry order rules


mirroringAutomata : List (Rule Block) -> Automata Block
mirroringAutomata rules =
    Automata.automata (Automata.horMirrorSymmetry identity) order rules


step : Automata Block -> Grid -> (Location -> Maybe Block -> Maybe Block)
step =
    Automata.step
