module View.CancelButton exposing (cancelButtonView)

import Html exposing (Html)
import Html.Attributes exposing (class)
import View.Widget exposing (dangerButtonViewWrapper)


cancelButtonView : cancelMsg -> Html cancelMsg
cancelButtonView msg =
    dangerButtonViewWrapper
        [ class "cancel" ]
        "cancel"
        msg
