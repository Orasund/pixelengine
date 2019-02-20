module VerifyExamples.Grid.Dimensions0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Grid exposing (..)



grid : Grid a
grid =
    empty dim
dim : { columns:Int , rows:Int }
dim =
    { columns=42
    , rows=3
    }



spec0 : Test.Test
spec0 =
    Test.test "#dimensions: \n\n    grid |> dimensions\n    --> dim" <|
        \() ->
            Expect.equal
                (
                grid |> dimensions
                )
                (
                dim
                )