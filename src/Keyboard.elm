module Keyboard exposing (view)

import Html exposing (Html, br, button, div, text)
import Html.Attributes exposing (class, type_)
import Html.Events exposing (on)
import Json.Decode as Decode


keyboardKey : (String -> msg) -> Char -> Html msg
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
        button [ type_ "button", class "keyboard__key", class "keyboard__key--wide", onPointerDown (toMsg letter) ] [ text letter ]

    else if letter == "↵" then
        button [ type_ "button", class "keyboard__key", class "keyboard__key--wide", class "keyboard__key--dark", onPointerDown (toMsg letter) ] [ text letter ]

    else
        text ""


view : (String -> msg) -> Html msg
view toMsg =
    div [ class "keyboard" ]
        [ div [ class "keyboard__keys" ]
            (List.map
                (keyboardKey toMsg)
                [ 'q', 'w', 'e', 'r', 't', 'y', 'u', 'i', 'o', 'p', '_', 'a', 's', 'd', 'f', 'g', 'h', 'j', 'k', 'l', '_', '↵', 'z', 'x', 'c', 'v', 'b', 'n', 'm', '⌫' ]
            )
        ]


onPointerDown : msg -> Html.Attribute msg
onPointerDown message =
    on "pointerdown" (Decode.succeed message)
