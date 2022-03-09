module Dictionary exposing (toList)

import Dict exposing (Dict)


type alias Solutions =
    Dict String Score


type alias Score =
    { scrabbleScore : Int, matchScore : Float }


toList : Solutions -> List { word : String, score : String }
toList solutions =
    solutions
        |> Dict.toList
        |> List.sortBy (\( _, score_ ) -> score_.matchScore)
        |> List.take 500
        |> List.map (\( word, score_ ) -> { word = word, score = score_.scrabbleScore |> String.fromInt })
