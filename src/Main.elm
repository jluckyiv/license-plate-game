port module Main exposing (..)

import Browser exposing (Document)
import Browser.Events
import Dict exposing (Dict)
import Element exposing (Element, alignBottom, centerX, centerY, column, el, fill, fillPortion, focusStyle, focused, height, inFront, link, maximum, mouseDown, mouseOver, moveLeft, none, padding, paddingXY, paragraph, pointer, px, rgb255, rgba255, row, scrollbarY, shrink, spacing, spacingXY, table, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events exposing (onClick, onMouseDown)
import Element.Font as Font exposing (center)
import Element.Input exposing (button)
import FontAwesome.Attributes exposing (border)
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


listSolutions : Solutions -> List { word : String, score : String }
listSolutions solutions =
    solutions
        |> Dict.toList
        |> List.sortBy (\( _, score_ ) -> score_.matchScore)
        |> List.take 500
        |> List.map (\( word, score_ ) -> { word = word, score = score_.scrabbleScore |> String.fromInt })


type alias Model =
    { inputValue : String
    , licensePlate : LicensePlate
    , validSolutions : Solutions
    , shouldShowSolutions : Bool
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
    = BlurredInput String
    | ClickedResetButton
    | GotRandomPlate LicensePlate
    | GotPlateCheck (List Result)
    | PressedKeyboardKey String
    | PressedShowSolutions
    | ClickedSolutions


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

        PressedShowSolutions ->
            ( { model | shouldShowSolutions = not model.shouldShowSolutions }, Cmd.none )

        ClickedSolutions ->
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


black =
    rgb255 17 14 21


yellow =
    rgb255 221 163 56


view : Model -> Document Msg
view model =
    { title = "Deb's License Plate Game"
    , body =
        [ Element.layout []
            (column
                [ width fill
                , height fill
                , Background.color (rgba255 17 14 21 0.01)
                ]
                [ licensePlateContainer model
                , gamePlayContainer model
                , keyboardContainer PressedKeyboardKey
                ]
            )
        ]
    }


licensePlateContainer : Model -> Element msg
licensePlateContainer model =
    el [ width fill, padding 10 ] (licensePlateView model)


licensePlateView : Model -> Element msg
licensePlateView model =
    column
        [ centerX
        , centerY
        , height (px 180)
        , width (px 360)
        , padding 20
        , spacing 20
        , Border.rounded 15
        , Background.color black
        , Font.family [ Font.typeface "LICENSE PLATE USA Regular" ]
        , Font.color yellow
        ]
        [ licensePlateState
        , licensePlateText model
        ]


licensePlateState : Element msg
licensePlateState =
    el [ centerX ] (text "CALIFORNIA")


licensePlateText : Model -> Element msg
licensePlateText model =
    el
        [ centerX
        , centerY
        , Font.size 70
        ]
        (text <| LicensePlate.toString model.licensePlate)


gamePlayContainer : Model -> Element Msg
gamePlayContainer model =
    column
        [ centerX
        , centerY
        , width fill
        , height fill
        , Element.inFront (viewSolutions model)
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
                            [ Font.color yellow
                            , Background.color (rgba255 14 14 21 0.9)
                            , Font.color yellow
                            , paddingXY 6 6
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
            (model.inputValue
                |> String.toList
                |> List.map letterTile
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
            Dict.get model.inputValue model.validSolutions
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
        [ height (px 58)
        , width (px 34)
        , Background.color (rgba255 221 163 56 0.5)
        , Border.rounded 4
        , Font.size 25
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
                , Background.color (rgba255 255 255 255 0.95)
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
            , Background.color (rgba255 255 255 255 0.95)
            , onClick ClickedSolutions
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
        {--
        el
            [ Background.color (rgb255 255 255 255)
            , centerX
            , height (px 250)
            , Border.solid
            , Border.width 1
            , Border.rounded 4
            , Border.color black
            , padding 10
            , Font.color black
            , Events.onClick ClickedSolutions
            ]
            (table
                [ width shrink
                , spacing 10
                , centerX
                , scrollbarY
                , height (px 250)
                ]
                { data = listSolutions solutions
                , columns =
                    [ { header = none
                      , width = fillPortion 3
                      , view = .word >> text >> el [ Font.alignLeft ]
                      }
                    , { header = none
                      , width = fillPortion 1
                      , view = .score >> String.fromInt >> text >> el [ Font.alignRight ]
                      }
                    ]
                }
            )
--}

    else
        text ""


keyboardContainer : (String -> Msg) -> Element Msg
keyboardContainer toMsg =
    el
        [ width fill
        , Background.color (rgb255 17 14 21)
        , Border.shadow
            { offset = ( 0.0, 0.0 )
            , size = 0
            , blur = 50
            , color = rgba255 0 0 0 0.5
            }
        ]
        (keyboard toMsg)


keyboard : (String -> Msg) -> Element Msg
keyboard toMsg =
    column
        [ Font.color (rgba255 221 163 56 0.9)
        , spacing 5
        , padding 5
        , centerX
        ]
        [ el [ centerX ] (row [ spacing 5 ] (keyboardRow toMsg "QWERTYUIOP"))
        , el [ centerX ] (row [ spacing 5 ] (keyboardRow toMsg "ASDFGHJKL"))
        , el [ centerX ]
            (row [ spacing 5 ]
                (List.concat
                    [ [ specialKey toMsg '↵' ]
                    , keyboardRow toMsg "ZXCVBNM"
                    , [ specialKey toMsg '⌫' ]
                    ]
                )
            )
        ]


specialKey : (String -> Msg) -> Char -> Element Msg
specialKey toMsg char =
    let
        letter =
            char |> String.fromChar |> String.toLower
    in
    button
        [ height (px 58)
        , width (px 48)
        , Background.color (rgba255 255 255 255 0.1)
        , Border.rounded 4
        , Font.size 25
        , focused []
        , mouseDown [ Background.color (rgba255 255 255 255 0.05) ]
        ]
        { label = el [ centerX, centerY ] (text letter)
        , onPress = Just (toMsg letter)
        }


keyboardRow : (String -> Msg) -> String -> List (Element Msg)
keyboardRow toMsg keys =
    keys |> String.toList |> List.map (keyboardKey toMsg)


keyboardKey : (String -> Msg) -> Char -> Element Msg
keyboardKey toMsg char =
    let
        letter =
            char |> String.fromChar |> String.toLower
    in
    button
        [ height (px 58)
        , width (px 32)
        , Background.color (rgba255 255 255 255 0.2)
        , Border.rounded 4
        , Font.size 25
        , focused []
        , mouseDown [ Background.color (rgba255 255 255 255 0.1) ]
        ]
        { label = el [ centerX, centerY ] (text letter)
        , onPress = Just (toMsg letter)
        }
