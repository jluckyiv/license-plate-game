module Keyboard exposing (view)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Html
import Html.Events exposing (on)
import Json.Decode as Decode
import Palette exposing (..)


view : (String -> msg) -> Element msg
view toMsg =
    el
        [ width fill
        , Background.color black
        , Border.shadow
            { offset = ( 0.0, 0.0 )
            , size = 0
            , blur = 50
            , color = black |> withAlpha 0.5
            }
        ]
        (keyboard toMsg)


keyboard : (String -> msg) -> Element msg
keyboard toMsg =
    column
        [ Font.color (yellow |> withAlpha 0.9)
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


specialKey : (String -> msg) -> Char -> Element msg
specialKey toMsg char =
    let
        letter =
            char |> String.fromChar |> String.toLower
    in
    button
        [ height (px 58)
        , width (px 48)
        , Background.color (white |> withAlpha 0.1)
        , Border.rounded 4
        , Font.size 25
        , focused []
        , mouseDown [ Background.color (white |> withAlpha 0.05) ]
        ]
        { label = el [ centerX, centerY ] (text letter)
        , onPress = Just (toMsg letter)
        }


keyboardRow : (String -> msg) -> String -> List (Element msg)
keyboardRow toMsg keys =
    keys |> String.toList |> List.map (keyboardKey toMsg)


keyboardKey : (String -> msg) -> Char -> Element msg
keyboardKey toMsg char =
    let
        letter =
            char |> String.fromChar |> String.toLower
    in
    button
        [ height (px 58)
        , width (px 32)
        , Background.color (white |> withAlpha 0.2)
        , Border.rounded 4
        , Font.size 25
        , focused []
        , mouseDown [ Background.color (white |> withAlpha 0.1) ]
        , toMsg letter |> onPointerDown |> htmlAttribute
        ]
        { label = el [ centerX, centerY ] (text letter)
        , onPress = Nothing
        }


onPointerDown : msg -> Html.Attribute msg
onPointerDown message =
    on "pointerdown" (Decode.succeed message)
