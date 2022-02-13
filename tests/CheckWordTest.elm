module CheckWordTest exposing (..)

import CheckWord
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "CheckWord"
        [ test "Test" <|
            \_ ->
                Expect.equal (CheckWord.isSubsequenceOf "TCE" "TRACES") True
        ]
