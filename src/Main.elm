port module Main exposing (..)

import Browser exposing (Document)
import Browser.Events
import Dict exposing (Dict)
import Element exposing (Element, alignBottom, centerX, centerY, column, el, fill, height, padding, px, rgb255, row, spacing, spacingXY, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
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


view : Model -> Document Msg
view model =
    { title = "Deb's License Plate Game"
    , body =
        [ Element.layout []
            (column
                [ width fill
                , height fill
                , Element.explain Debug.todo
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
        , Background.color (rgb255 17 14 21)
        , Font.family [ Font.typeface "LICENSE PLATE USA Regular" ]
        , Font.color (rgb255 221 163 56)
        ]
        [ licensePlateState
        , licensPlateText model
        ]


licensePlateState : Element msg
licensePlateState =
    el [ centerX ] (text "CALIFORNIA")


licensPlateText : Model -> Element msg
licensPlateText model =
    el
        [ centerX
        , centerY
        , Font.size 70
        ]
        (text <| LicensePlate.toString model.licensePlate)


gamePlayContainer : Model -> Element msg
gamePlayContainer model =
    el [ centerX, centerY, width fill, height fill ] (text "Gameplay container")


keyboardContainer : (String -> Msg) -> Element Msg
keyboardContainer toMsg =
    el [ centerX, centerY ] (keyboard toMsg)


keyboard : (String -> Msg) -> Element Msg
keyboard toMsg =
    text "Keyboard container"
