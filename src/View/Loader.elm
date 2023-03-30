module View.Loader exposing (loaderSettingButtonView, loaderView)

import Html exposing (Html, div, input)
import Html.Attributes exposing (class)
import Html.Events exposing (onInput)
import View.Widget exposing (buttonViewWrapper)


loaderSettingButtonView : msg -> Html msg
loaderSettingButtonView loadMsg =
    buttonViewWrapper
        [ class "loader-setting" ]
        "load"
        loadMsg


loaderView : (String -> msg) -> msg -> Html msg
loaderView inputHandler fetchMsg =
    div
        [ class "loader-wrapper" ]
        [ input
            [ onInput inputHandler ]
            []
        , buttonViewWrapper
            [ class "fetch-source" ]
            "fetch"
            fetchMsg
        ]
