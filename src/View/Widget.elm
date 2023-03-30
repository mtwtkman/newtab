module View.Widget exposing (buttonViewWrapper, inputViewWrapper)

import Html exposing (Attribute, Html, button, div, input, text)
import Html.Attributes exposing (class, value)
import Html.Events exposing (onClick, onInput)


buttonViewWrapper : List (Attribute msg) -> String -> msg -> Html msg
buttonViewWrapper attrs label msg =
    div
        attrs
        [ button
            [ onClick msg ]
            [ text label ]
        ]


inputViewWrapper : String -> String -> (String -> msg) -> Html msg
inputViewWrapper label inputValue handler =
    div
        [ class ("input-" ++ label) ]
        [ input
            [ value inputValue
            , onInput handler
            ]
            []
        ]
