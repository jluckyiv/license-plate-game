module Palette exposing (black, white, withAlpha, yellow)

import Element exposing (Color, fromRgb, rgb255, toRgb)


black : Color
black =
    rgb255 17 14 21


yellow : Color
yellow =
    rgb255 221 163 56


white : Color
white =
    rgb255 0xF9 0xFA 0xFB


withAlpha : Float -> Color -> Color
withAlpha alpha color =
    color
        |> toRgb
        |> (\rgb_ -> { rgb_ | alpha = alpha })
        |> fromRgb
