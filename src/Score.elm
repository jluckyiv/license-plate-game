module Score exposing (Score(..), noScore, score, toString)

import CheckWord exposing (CheckWord(..))
import LicensePlate exposing (LicensePlate)
import ScrabbleScore


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
  let
      wordToCheck = LicensePlate.letters plate
  in
  
    case ( isValidWord, CheckWord.isSubsequenceOf wordToCheck  word) of
        ( True, True ) ->
            Valid (ScrabbleScore.score word)

        ( False, True ) ->
            Invalid [ InvalidWord ]

        ( True, False ) ->
            Invalid [ InvalidSequence ]

        ( False, False ) ->
            Invalid [ InvalidWord, InvalidSequence ]
