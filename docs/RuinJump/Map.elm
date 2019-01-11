module RuinJump.Map exposing (Map,remove)

import RuinJump.MapElement as MapElement exposing (Block(..), MapElement(..))
import RuinJump.Config as Config
import Dict exposing (Dict)
import Random exposing (Generator)

type alias Map =
    Dict ( Int, Int ) MapElement

remove : (Int,Int) -> Map -> Map
remove (x,lowestY) map =
    map
        |> Dict.update
            ( x, lowestY )
            (Maybe.map MapElement.remove)
        |> (if
                lowestY
                    |> modBy Config.sectionHeight
                    |> (==) 0
            then
                Dict.filter (\( _, y ) _ -> y <= lowestY)

            else
                identity
            )