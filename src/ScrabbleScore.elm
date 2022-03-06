module ScrabbleScore exposing (score)


score : String -> Int
score word =
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
        onePoint =
            [ 'A', 'E', 'I', 'L', 'N', 'O', 'R', 'S', 'T', 'U' ]

        twoPoints =
            [ 'D', 'G' ]

        threePoints =
            [ 'B', 'C', 'M', 'P' ]

        fourPoints =
            [ 'F', 'H', 'V', 'W', 'Y' ]

        fivePoints =
            [ 'K' ]

        eightPoints =
            [ 'J', 'X' ]

        tenPoints =
            [ 'Q', 'Z' ]
    in
    if List.member char onePoint then
        1

    else if List.member char twoPoints then
        2

    else if List.member char threePoints then
        3

    else if List.member char fourPoints then
        4

    else if List.member char fivePoints then
        5

    else if List.member char eightPoints then
        8

    else if List.member char tenPoints then
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
