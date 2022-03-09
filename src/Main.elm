port module Main exposing (..)

import Browser exposing (Document)
import Browser.Events
import Dict exposing (Dict)
import Element
    exposing
        ( Element
        , alignBottom
        , centerX
        , centerY
        , column
        , el
        , fill
        , fillPortion
        , focused
        , height
        , htmlAttribute
        , none
        , padding
        , paddingXY
        , pointer
        , px
        , row
        , scrollbarY
        , spacing
        , table
        , text
        , width
        , wrappedRow
        )
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input exposing (button)
import Html.Attributes exposing (class)
import Http exposing (Error(..))
import Json.Decode as Decode
import Keyboard
import LicensePlate exposing (LicensePlate)
import Palette exposing (..)
import Process
import Random
import Score exposing (Score(..))
import ScrabbleScore
import Task



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


listSolutions : Solutions -> List { word : String, score : String }
listSolutions solutions =
    solutions
        |> Dict.toList
        |> List.sortBy (\( _, score_ ) -> score_.matchScore)
        |> List.take 500
        |> List.map (\( word, score_ ) -> { word = word, score = score_.scrabbleScore |> String.fromInt })


type InputValue
    = Stable String
    | Unstable String


type alias Model =
    { inputValue : InputValue
    , licensePlate : LicensePlate
    , validSolutions : Solutions
    , shouldShowSolutions : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        inputValue =
            Stable ""

        licensePlate =
            LicensePlate.empty

        validWords =
            Dict.empty

        shouldShowSolutions =
            False
    in
    ( Model inputValue licensePlate validWords shouldShowSolutions, getRandomPlate )


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
    = GotPlateCheck (List Result)
    | GotRandomPlate LicensePlate
    | ItsBeenASecond
    | PressedKeyboardKey String
    | PressedResetButton
    | PressedShowSolutions
    | PressedSolutionsButton


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PressedResetButton ->
            ( { model | inputValue = Stable "" }, getRandomPlate )

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

        ItsBeenASecond ->
            case model.inputValue of
                Stable _ ->
                    ( model, Cmd.none )

                Unstable value ->
                    ( { model | inputValue = Stable (String.dropRight 1 value) }, Cmd.none )

        PressedShowSolutions ->
            ( { model | shouldShowSolutions = not model.shouldShowSolutions }, Cmd.none )

        PressedSolutionsButton ->
            ( { model | shouldShowSolutions = not model.shouldShowSolutions }, Cmd.none )


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
            case model.inputValue of
                Stable value ->
                    ( { model | inputValue = Unstable value }, Task.perform (\_ -> ItsBeenASecond) (Process.sleep 100) )

                Unstable value ->
                    ( { model | inputValue = Unstable (String.dropRight 1 value) }, Task.perform (\_ -> ItsBeenASecond) (Process.sleep 100) )

        Just ( 'B', "ackspace" ) ->
            case model.inputValue of
                Stable value ->
                    ( { model | inputValue = Unstable value }, Task.perform (\_ -> ItsBeenASecond) (Process.sleep 100) )

                Unstable value ->
                    ( { model | inputValue = Unstable (String.dropRight 1 value) }, Task.perform (\_ -> ItsBeenASecond) (Process.sleep 100) )

        Just ( char, "" ) ->
            case ( Char.isAlpha char, model.inputValue ) of
                ( True, Stable value ) ->
                    ( { model | inputValue = Stable (value ++ key) }, Cmd.none )

                ( True, Unstable value ) ->
                    ( { model | inputValue = Unstable (value ++ key) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


getRandomPlate : Cmd Msg
getRandomPlate =
    Random.generate GotRandomPlate LicensePlate.generator


view : Model -> Document Msg
view model =
    { title = "Deb's License Plate Game"
    , body =
        [ Element.layout []
            (column
                [ width fill
                , height fill
                , Background.color white
                ]
                [ LicensePlate.view model.licensePlate
                , gamePlayContainer model
                , Keyboard.view PressedKeyboardKey
                ]
            )
        ]
    }


gamePlayContainer : Model -> Element Msg
gamePlayContainer model =
    column
        [ centerX
        , centerY
        , width fill
        , height fill
        , Element.inFront
            (el
                [ paddingXY 7 0
                , width fill
                ]
                (viewSolutions model)
            )
        ]
        [ row
            [ centerX
            , width fill
            ]
            [ row
                [ centerX
                ]
                [ el
                    []
                    (row
                        [ spacing 10 ]
                        [ button
                            [ Font.color black
                            , Background.color (yellow |> withAlpha 0.9)
                            , Font.color black
                            , paddingXY 8 8
                            , Border.rounded 3
                            , focused []
                            ]
                            { label = text "Show"
                            , onPress = Just PressedShowSolutions
                            }
                        , text
                            ((model.validSolutions
                                |> Dict.toList
                                |> List.length
                                |> String.fromInt
                             )
                                ++ " solutions"
                            )
                        , button
                            [ Font.color yellow
                            , Background.color (black |> withAlpha 0.9)
                            , Font.color yellow
                            , paddingXY 8 8
                            , Border.rounded 3
                            , focused []
                            ]
                            { label = text "Reset"
                            , onPress = Just PressedResetButton
                            }
                        ]
                    )
                ]
            ]
        , wrappedRow
            [ centerX
            , centerY
            , paddingXY 8 6
            , spacing 4
            , height (px 50)
            ]
            (case model.inputValue of
                Stable value ->
                    value
                        |> String.toList
                        |> List.map letterTile

                Unstable value ->
                    case value |> String.reverse |> String.uncons of
                        Just ( last, rest ) ->
                            fadingLetterTile last
                                :: (rest |> String.toList |> List.map letterTile)
                                |> List.reverse

                        Nothing ->
                            []
            )
        , row
            [ centerX
            , alignBottom
            , padding 10
            , spacing 4
            ]
            [ text ("Score: " ++ (model |> score |> String.fromInt)) ]
        ]


score : Model -> Int
score model =
    let
        result =
            case model.inputValue of
                Stable value ->
                    Dict.get value model.validSolutions

                Unstable value ->
                    Dict.get value model.validSolutions
    in
    case result of
        Nothing ->
            0

        Just s ->
            s.scrabbleScore


letterTile : Char -> Element msg
letterTile char =
    let
        letter =
            char |> String.fromChar |> String.toUpper
    in
    el
        [ height (px 34)
        , width (px 34)
        , Background.color (yellow |> withAlpha 0.5)
        , Border.rounded 4
        , Font.size 25
        , class "popIn" |> htmlAttribute
        ]
        (el
            [ centerX
            , centerY
            ]
            (text letter)
        )


fadingLetterTile : Char -> Element msg
fadingLetterTile char =
    let
        letter =
            char |> String.fromChar |> String.toUpper
    in
    el
        [ height (px 34)
        , width (px 34)
        , Background.color (yellow |> withAlpha 0.5)
        , Border.rounded 4
        , Font.size 25
        , class "fadeOut" |> htmlAttribute
        ]
        (el
            [ centerX
            , centerY
            ]
            (text letter)
        )


viewSolutions : Model -> Element Msg
viewSolutions model =
    let
        solutions =
            model.validSolutions
                |> listSolutions
    in
    if model.shouldShowSolutions then
        let
            headerAttrs =
                [ Font.bold
                , Font.color black
                , Border.widthEach { bottom = 2, top = 0, left = 0, right = 0 }
                , Border.color black
                , Background.color white
                ]
        in
        column
            [ width fill
            , height fill
            , spacing 3
            , padding 10
            , Border.width 2
            , Border.rounded 6
            , Border.color black
            , Background.color white
            , onClick PressedSolutionsButton
            ]
            [ row [ width fill ]
                [ el ([ width <| fillPortion 1, Font.alignLeft, pointer ] ++ headerAttrs) <| text "X"
                , el ((width <| fillPortion 7) :: headerAttrs) <| text "Word"
                , el ([ width <| fillPortion 1, Font.alignRight ] ++ headerAttrs) <| text "Score"
                ]

            -- workaround for a bug: it's necessary to wrap `table` in an `el`
            -- to get table height attribute to apply
            , el [ width fill ] <|
                table
                    [ width fill
                    , height (px 250)
                    , scrollbarY
                    , spacing 5
                    ]
                    { data = solutions
                    , columns =
                        [ { header = none
                          , width = fillPortion 1
                          , view = always "" >> text >> el [ Font.alignLeft ]
                          }
                        , { header = none
                          , width = fillPortion 7
                          , view = .word >> text >> el [ centerY ]
                          }
                        , { header = none
                          , width = fillPortion 1
                          , view = .score >> text >> el [ Font.alignRight ]
                          }
                        ]
                    }
            ]

    else
        text ""
