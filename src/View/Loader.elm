module View.Loader exposing (loaderSettingButtonView, loaderView)

import Html exposing (Html, div)
import Html.Attributes exposing (class)
import View.CancelButton exposing (cancelButtonView)
import View.Widget exposing (infoButtonViewWrapper, inputViewWrapper)


loaderSettingButtonView : msg -> Html msg
loaderSettingButtonView loadMsg =
    infoButtonViewWrapper
        [ class "loader-setting" ]
        "load"
        loadMsg


loaderView : String -> (String -> msg) -> msg -> msg -> Html msg
loaderView inputValue inputHandler fetchMsg cancelMsg =
    div
        [ class "loader-wrapper" ]
        [ inputViewWrapper "loader" inputValue inputHandler
        , infoButtonViewWrapper
            [ class "fetch-source" ]
            "fetch"
            fetchMsg
        , cancelButtonView cancelMsg
        ]
