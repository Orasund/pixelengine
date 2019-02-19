module RuinJump.Map exposing (Map, remove)

import Dict exposing (Dict)
import Grid.Position exposing (Position)
import RuinJump.Config as Config
import RuinJump.MapElement as MapElement exposing (Block(..), MapElement(..))


type alias Map =
    Dict Position MapElement


remove : Position -> Map -> Map
remove (( _, lowestY ) as pos) map =
    map
        |> Dict.update pos (Maybe.map MapElement.remove)
        |> (if
                lowestY
                    |> modBy Config.sectionHeight
                    |> (==) 0
            then
                Dict.filter (\( _, y ) _ -> y <= lowestY)

            else
                identity
           )
