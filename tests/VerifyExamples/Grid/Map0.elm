module VerifyExamples.Grid.Map0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Grid exposing (..)



dimensions : { columns:Int , rows:Int }
dimensions =
    { columns=42
    , rows=3
    }



spec0 : Test.Test
spec0 =
    Test.test "#map: \n\n    empty dimensions |> map (\\_ _ -> Just 42)\n    --> fill (always <| Just 42) dimensions" <|
        \() ->
            Expect.equal
                (
                empty dimensions |> map (\_ _ -> Just 42)
                )
                (
                fill (always <| Just 42) dimensions
                )