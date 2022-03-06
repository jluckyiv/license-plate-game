module CheckWord exposing (..)

import List.Extra as List


type CheckWord
    = Valid String
    | Invalid String


create : String -> String -> CheckWord
create needle haystack =
    if isSubsequenceOf needle haystack then
        Valid needle

    else
        Invalid needle


isSubsequenceOf : String -> String -> Bool
isSubsequenceOf needle haystack =
    let
        first =
            toUppercaseList needle

        second =
            toUppercaseList haystack
    in
    List.isSubsequenceOf first second


toUppercaseList : String -> List Char
toUppercaseList str =
    str
        |> String.toUpper
        |> String.toList
