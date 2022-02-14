port module Main exposing (..)

import Browser exposing (Document)
import CheckWord exposing (Score(..))
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Http exposing (Error(..))
import LicensePlate exposing (LicensePlate)
import Random



-- PORTS


port getWordCheck : String -> Cmd msg


port gotWordCheck : (Bool -> msg) -> Sub msg


type alias Model =
    { inputValue : String
    , licensePlate : LicensePlate
    , score : Score
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        inputValue =
            ""

        licensePlate =
            LicensePlate.empty

        score =
            CheckWord.score False licensePlate inputValue
    in
    ( Model inputValue licensePlate score, getRandomPlate )


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    gotWordCheck GotWordCheck


type Msg
    = BlurredInput String
    | ClickedResetButton
    | GotRandomPlate LicensePlate
    | GotWordCheck Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BlurredInput str ->
            ( { model | inputValue = str }, getScrabbleScore str )

        ClickedResetButton ->
            ( { model | inputValue = "" }, getRandomPlate )

        GotRandomPlate plate ->
            ( { model | licensePlate = plate }, Cmd.none )

        GotWordCheck isValid ->
            let
                score =
                    CheckWord.score isValid model.licensePlate model.inputValue
            in
            ( { model | score = score }, Cmd.none )


getRandomPlate : Cmd Msg
getRandomPlate =
    Random.generate GotRandomPlate LicensePlate.generator


getScrabbleScore : String -> Cmd Msg
getScrabbleScore str =
    getWordCheck str


view : Model -> Document Msg
view model =
    { title = "Title"
    , body =
        [ div []
            [ div [ class "license-plate-state" ] [ text "CALIFORNIA" ]
            , div [ class "license-plate" ] [ text <| LicensePlate.toString model.licensePlate ]
            ]
        , div []
            [ input [ placeholder "Word", value model.inputValue, onInput BlurredInput ] []
            , text <| "Score: " ++ CheckWord.toString model.score
            ]
        , button [ onClick ClickedResetButton ] [ text "Reset" ]
        ]
    }
