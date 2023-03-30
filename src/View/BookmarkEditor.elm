module View.BookmarkEditor exposing (bookmarkEditorView)

import Entity exposing (Bookmark, Title, Url)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import View.Widget exposing (buttonViewWrapper, inputViewWrapper)


type alias ToMsg subMsg msg =
    subMsg -> msg


type alias InputTitle subMsg =
    Title -> subMsg


type alias InputUrl subMsg =
    Url -> subMsg


bookmarkEditorView : ToMsg subMsg msg -> InputTitle subMsg -> InputUrl subMsg -> msg -> msg -> Bookmark -> Html msg
bookmarkEditorView liftSubMsg inputTitleHandler inputUrlHandler saveMsg cancelMsg bookmark =
    div
        [ class "bookmark-editor" ]
        [ inputFormView liftSubMsg inputTitleHandler inputUrlHandler bookmark
        , saveButtonView saveMsg
        , cancelButtonView cancelMsg
        ]


inputFormView : ToMsg subMsg msg -> InputTitle subMsg -> InputUrl subMsg -> Bookmark -> Html msg
inputFormView toMsg inputTitleMsg inputUrlMsg bookmark =
    div
        [ class "input-form" ]
        [ Html.map toMsg (inputViewWrapper "title" bookmark.title inputTitleMsg)
        , Html.map toMsg (inputViewWrapper "url" bookmark.url inputUrlMsg)
        ]


saveButtonView : saveMsg -> Html saveMsg
saveButtonView msg =
    buttonViewWrapper
        [ class "save" ]
        "save"
        msg


cancelButtonView : cancelMsg -> Html cancelMsg
cancelButtonView msg =
    buttonViewWrapper
        [ class "cancel" ]
        "cancel"
        msg
