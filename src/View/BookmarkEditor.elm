module View.BookmarkEditor exposing
    ( Model(..)
    , Msg(..)
    , initModel
    , update
    , view
    )

import Entity exposing (Bookmark, Title, Url)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import View.CancelButton exposing (cancelButtonView)
import View.Widget exposing (dangerButtonViewWrapper, infoButtonViewWrapper, inputViewWrapper)


type Model
    = Editting Bookmark
    | Canceled
    | Saved Bookmark


initModel : Bookmark -> Model
initModel =
    Editting


type Msg
    = InputUrl Url
    | InputTitle Title
    | Save
    | Cancel


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( InputUrl newUrl, Editting bookmark ) ->
            ( Editting { bookmark | url = newUrl }, Cmd.none )

        ( InputTitle newTitle, Editting bookmark ) ->
            ( Editting { bookmark | title = newTitle }, Cmd.none )

        ( Save, Editting bookmark ) ->
            ( Saved bookmark, Cmd.none )

        ( Cancel, Editting _ ) ->
            ( Canceled, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case model of
        Editting bookmark ->
            div
                [ class "bookmark-editor" ]
                [ inputFormView bookmark
                , saveButtonView
                , cancelButtonView
                ]

        _ ->
            div [] []


inputFormView : Bookmark -> Html Msg
inputFormView bookmark =
    div
        [ class "input-form" ]
        [ inputViewWrapper "title" bookmark.title InputTitle
        , inputViewWrapper "url" bookmark.url InputUrl
        ]


saveButtonView : Html Msg
saveButtonView =
    infoButtonViewWrapper
        [ class "save" ]
        "save"
        Save


cancelButtonView : Html Msg
cancelButtonView =
    dangerButtonViewWrapper
        [ class "cancel" ]
        "cancel"
        Cancel
