module CheckWord exposing (..)

import LicensePlate exposing (LicensePlate)
import List.Extra as List exposing (isSubsequenceOf)


type Score
    = NotRequested
    | Valid Int
    | Invalid (List Reason)


type Reason
    = InvalidSequence
    | InvalidWord


noScore : Score
noScore =
    NotRequested


toString : Score -> String
toString score_ =
    case score_ of
        NotRequested ->
            ""

        Valid int ->
            String.fromInt int

        Invalid list ->
            list
                |> List.map reasonToString
                |> String.join ", "


reasonToString : Reason -> String
reasonToString reason =
    case reason of
        InvalidSequence ->
            "Sequence is invalid"

        InvalidWord ->
            "Word is invalid"


score : Bool -> LicensePlate -> String -> Score
score isValidWord plate word =
    case ( isValidWord, isSubsequenceOf (LicensePlate.letters plate) word ) of
        ( True, True ) ->
            Valid (scoreWord word)

        ( False, True ) ->
            Invalid [ InvalidWord ]

        ( True, False ) ->
            Invalid [ InvalidSequence ]

        ( False, False ) ->
            Invalid [ InvalidWord, InvalidSequence ]


scoreWord : String -> Int
scoreWord word =
    let
        chars =
            word |> String.toUpper |> String.toList
    in
    chars
        |> List.map scoreChar
        |> List.sum


scoreChar : Char -> Int
scoreChar char =
    let
        one =
            [ 'A', 'E', 'I', 'L', 'N', 'O', 'R', 'S', 'T', 'U' ]

        two =
            [ 'D', 'G' ]

        three =
            [ 'B', 'C', 'M', 'P' ]

        four =
            [ 'F', 'H', 'V', 'W', 'Y' ]

        five =
            [ 'K' ]

        eight =
            [ 'J', 'X' ]

        ten =
            [ 'Q', 'Z' ]
    in
    if List.member char one then
        1

    else if List.member char two then
        2

    else if List.member char three then
        3

    else if List.member char four then
        4

    else if List.member char five then
        5

    else if List.member char eight then
        8

    else if List.member char ten then
        10

    else
        0



{--
  Here are the point values for each letter in Scrabble.
0 Points - Blank tile.
1 Point - A, E, I, L, N, O, R, S, T and U.
2 Points - D and G.
3 Points - B, C, M and P.
4 Points - F, H, V, W and Y.
5 Points - K.
8 Points - J and X.
10 Points - Q and Z.

  --}


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
