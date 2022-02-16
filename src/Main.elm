port module Main exposing (..)

import Browser exposing (Document)
import CheckWord exposing (Score(..))
import Html exposing (Html, br, button, div, input, span, text)
import Html.Attributes exposing (class, placeholder, type_, value)
import Html.Events exposing (on, onClick, onInput)
import Http exposing (Error(..))
import Json.Decode as Decode
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
            CheckWord.noScore
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
    | PressedKeyboardKey String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BlurredInput str ->
            ( { model | inputValue = str }, Cmd.none )

        ClickedResetButton ->
            ( { model | inputValue = "", score = CheckWord.noScore }, getRandomPlate )

        GotRandomPlate plate ->
            ( { model | licensePlate = plate }, Cmd.none )

        GotWordCheck isValid ->
            let
                score =
                    CheckWord.score isValid model.licensePlate model.inputValue
            in
            ( { model | score = score }, Cmd.none )

        PressedKeyboardKey key ->
            handleKeyboardKey model key


handleKeyboardKey : Model -> String -> ( Model, Cmd Msg )
handleKeyboardKey model key =
    case key of
        "↵" ->
            ( model, getScrabbleScore model.inputValue )

        "⌫" ->
            ( { model
                | inputValue = String.dropRight 1 model.inputValue
                , score = CheckWord.noScore
              }
            , Cmd.none
            )

        _ ->
            ( { model
                | inputValue = model.inputValue ++ key
                , score = CheckWord.noScore
              }
            , Cmd.none
            )


getRandomPlate : Cmd Msg
getRandomPlate =
    Random.generate GotRandomPlate LicensePlate.generator


getScrabbleScore : String -> Cmd Msg
getScrabbleScore str =
    getWordCheck str


onPointerDown : msg -> Html.Attribute msg
onPointerDown message =
    on "pointerdown" (Decode.succeed message)


keyboardKey : (String -> Msg) -> Char -> Html Msg
keyboardKey toMsg char =
    let
        letter =
            String.fromChar char
    in
    if Char.isAlpha char then
        button [ type_ "button", class "keyboard__key", onPointerDown (toMsg letter) ] [ text letter ]

    else if letter == "_" then
        br [] []

    else if letter == "⌫" then
        button [ type_ "button", class "keyboard__key", class "keyboard__key--wide", onClick (toMsg letter) ] [ text letter ]

    else if letter == "↵" then
        button [ type_ "button", class "keyboard__key", class "keyboard__key--wide", class "keyboard__key--dark", onClick (toMsg letter) ] [ text letter ]

    else
        text ""


scrabbleTile : Char -> Html msg
scrabbleTile char =
    let
        letter =
            String.fromChar char
    in
    span [ class "scrabble-tile__letter" ] [ text letter ]


viewScore : Score -> Html Msg
viewScore score =
    case score of
        CheckWord.NotRequested ->
            text ""

        _ ->
            div []
                [ div [ class "score" ]
                    [ text <| CheckWord.toString score
                    ]
                , div []
                    [ button [ class "reset-button", onClick ClickedResetButton ] [ text "Reset" ]
                    ]
                ]


view : Model -> Document Msg
view model =
    { title = "Deb's License Plate Game"
    , body =
        [ div [ class "container" ]
            [ div [ class "license-plate" ]
                [ div [ class "license-plate__state" ] [ text "CALIFORNIA" ]
                , div [ class "license-plate__number" ] [ text <| LicensePlate.toString model.licensePlate ]
                ]
            , div [ class "board" ]
                [ div [ class "scrabble-container", class "display--inline-block" ]
                    (model.inputValue
                        |> String.toList
                        |> List.map scrabbleTile
                    )
                ]
            , viewScore model.score
            , div [ class "keyboard" ]
                [ div [ class "keyboard__keys" ]
                    (List.map
                        (keyboardKey PressedKeyboardKey)
                        [ 'q', 'w', 'e', 'r', 't', 'y', 'u', 'i', 'o', 'p', '_', 'a', 's', 'd', 'f', 'g', 'h', 'j', 'k', 'l', '_', '↵', 'z', 'x', 'c', 'v', 'b', 'n', 'm', '⌫' ]
                    )
                ]
            ]
        ]
    }
