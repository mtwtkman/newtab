module View.Widget exposing (inputViewWrapper, infoButtonViewWrapper, dangerButtonViewWrapper, successButtonViewWrapper)

import Html exposing (Attribute, Html, button, div, input, text)
import Html.Attributes exposing (class, value)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (title)
import Html.Attributes exposing (type_)


type ButtonColor
  = DangerButton
  | InfoButton
  | SuccessButton


buttonColorToClassName : ButtonColor -> Attribute msg
buttonColorToClassName buttonColor =
  case buttonColor of
    DangerButton -> class "is-danger"
    InfoButton -> class "is-info"
    SuccessButton -> class "is-success"

buttonViewWrapper : ButtonColor -> List (Attribute msg) -> String -> msg -> Html msg
buttonViewWrapper buttonColor attrs label msg =
    div
        attrs
        [ button
            [ onClick msg
            , class "button"
            , class "is-light"
            , class "is-rounded"
            , buttonColorToClassName buttonColor
            , type_ "button"
            ]
            [ text label ]
        ]


infoButtonViewWrapper : List (Attribute msg) -> String -> msg -> Html msg
infoButtonViewWrapper = buttonViewWrapper InfoButton

dangerButtonViewWrapper : List (Attribute msg) -> String -> msg -> Html msg
dangerButtonViewWrapper = buttonViewWrapper DangerButton


successButtonViewWrapper : List (Attribute msg) -> String -> msg -> Html msg
successButtonViewWrapper = buttonViewWrapper SuccessButton

inputViewWrapper : String -> String -> (String -> msg) -> Html msg
inputViewWrapper label inputValue handler =
    div
        [ class ("input-" ++ label)
        ]
        [ input
            [ value inputValue
            , title label
            , onInput handler
            , class "input"
            , type_ "text"
            ]
            []
        ]
