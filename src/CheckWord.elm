module CheckWord exposing (..)

import List.Extra as List


toUppercaseList : String -> List Char
toUppercaseList str =
    str
        |> String.toUpper
        |> String.toList


isSubsequenceOf : String -> String -> Bool
isSubsequenceOf seq wor =
    let
        sequence =
            toUppercaseList seq

        word =
            toUppercaseList wor
    in
    List.isSubsequenceOf sequence word
