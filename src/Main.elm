port module Main exposing (..)

import Browser exposing (Document)
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html, br, button, div, input, p, span, text)
import Html.Attributes exposing (class, placeholder, style, type_, value)
import Html.Events exposing (on, onClick, onInput)
import Http exposing (Error(..))
import Json.Decode as Decode
import Keyboard
import LicensePlate exposing (LicensePlate)
import Random
import Score exposing (Score(..))
import ScrabbleScore



-- PORTS


port getPlateCheck : String -> Cmd msg


port gotPlateCheck : (List Result -> msg) -> Sub msg


type alias Result =
    { word : String
    , score : Float
    }


type alias Solutions =
    Dict String Score


type alias Score =
    { scrabbleScore : Int, matchScore : Float }


type alias Model =
    { inputValue : String
    , licensePlate : LicensePlate
    , validSolutions : Solutions
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        inputValue =
            ""

        licensePlate =
            LicensePlate.empty

        validWords =
            Dict.empty
    in
    ( Model inputValue licensePlate validWords, getRandomPlate )


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
    Sub.batch
        [ gotPlateCheck GotPlateCheck
        , Browser.Events.onKeyDown (Decode.map PressedKeyboardKey (Decode.field "key" Decode.string))
        ]


type Msg
    = BlurredInput String
    | ClickedResetButton
    | GotRandomPlate LicensePlate
    | GotPlateCheck (List Result)
    | PressedKeyboardKey String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BlurredInput str ->
            ( { model | inputValue = str }, Cmd.none )

        ClickedResetButton ->
            ( { model | inputValue = "" }, getRandomPlate )

        GotRandomPlate plate ->
            ( { model | licensePlate = plate }, getPlateCheck (LicensePlate.letters plate) )

        GotPlateCheck resultList ->
            let
                cmd =
                    case resultList of
                        [] ->
                            getRandomPlate

                        _ ->
                            Cmd.none
            in
            ( { model | validSolutions = toSolutions resultList }, cmd )

        PressedKeyboardKey key ->
            handleKeyboardKey model key


toSolutions : List Result -> Solutions
toSolutions words =
    words
        |> List.map
            (\result ->
                ( result.word
                , { scrabbleScore = ScrabbleScore.score result.word
                  , matchScore = result.score
                  }
                )
            )
        |> Dict.fromList


type Key
    = Character Char
    | Control String
    | Backspace
    | Enter


keyDecoder : Decode.Decoder Key
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)


toKey : String -> Key
toKey string =
    case String.uncons string of
        Just ( '↵', "" ) ->
            Enter

        Just ( '⌫', "" ) ->
            Backspace

        Just ( char, "" ) ->
            Character char

        _ ->
            Control string


handleKeyboardKey : Model -> String -> ( Model, Cmd Msg )
handleKeyboardKey model key =
    case String.uncons key of
        Just ( '↵', "" ) ->
            ( model, Cmd.none )

        Just ( 'E', "nter" ) ->
            ( model, Cmd.none )

        Just ( '⌫', "" ) ->
            ( { model
                | inputValue = String.dropRight 1 model.inputValue
              }
            , Cmd.none
            )

        Just ( 'B', "ackspace" ) ->
            ( { model
                | inputValue = String.dropRight 1 model.inputValue
              }
            , Cmd.none
            )

        Just ( char, "" ) ->
            if Char.isAlpha char then
                ( { model
                    | inputValue = model.inputValue ++ key
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


getRandomPlate : Cmd Msg
getRandomPlate =
    Random.generate GotRandomPlate LicensePlate.generator


letterTile : Char -> Html msg
letterTile char =
    let
        letter =
            String.fromChar char
    in
    span [ class "board__letter-tile" ] [ text letter ]


viewSolutions : Solutions -> Html Msg
viewSolutions solutions =
    let
        numberOfSolutions =
            String.fromInt
                (solutions
                    |> Dict.toList
                    |> List.length
                )
    in
    div []
        [ p []
            [ text
                ("There are " ++ numberOfSolutions ++ " valid solutions.")
            ]
        ]


viewPlayerSolution : String -> Solutions -> Html msg
viewPlayerSolution inputValue validSolutions =
    div []
        [ p []
            [ text <|
                if Dict.member inputValue validSolutions then
                    " is "

                else
                    " is not "
                        ++ "a valid solution."
            ]
        ]


viewScrabbleScore : String -> Html msg
viewScrabbleScore inputValue =
    div [] [ p [] [ text ("Scrabble score: " ++ String.fromInt (ScrabbleScore.score inputValue)) ] ]


view : Model -> Document Msg
view model =
    { title = "Deb's License Plate Game"
    , body =
        [ div [ class "container" ]
            [ LicensePlate.view model.licensePlate
            , viewSolutions model.validSolutions
            , div []
                [ div []
                    [ button [ class "reset-button", onClick ClickedResetButton ] [ text "Reset" ]
                    ]
                ]
            , div [ class "board" ]
                [ div [ style "min-height" "100px", style "height" "50px" ]
                    (model.inputValue
                        |> String.toList
                        |> List.map letterTile
                    )
                , div [ style "clear" "both" ] []
                ]
            , viewScrabbleScore model.inputValue
            , Keyboard.view PressedKeyboardKey
            ]
        ]
    }
