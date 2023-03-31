module View.CancelButton exposing (cancelButtonView)

import Html exposing (Html)
import Html.Attributes exposing (class)
import View.Widget exposing (buttonViewWrapper)


cancelButtonView : cancelMsg -> Html cancelMsg
cancelButtonView msg =
    buttonViewWrapper
        [ class "cancel" ]
        "cancel"
        msg
