module View.Export exposing (exportBookmarksView)

import Html exposing (Html, div)
import Html.Attributes exposing (class)
import View.Widget exposing (buttonViewWrapper)


exportBookmarksButton : msg -> Html msg
exportBookmarksButton exportMsg =
    buttonViewWrapper
        [ class "export-bookmarks-button" ]
        "export"
        exportMsg


exportBookmarksView : msg -> Html msg
exportBookmarksView exportMsg =
    div
        [ class "export-bookmarks-wrapper" ]
        [ exportBookmarksButton exportMsg
        ]
