module View.BookmarkEditor exposing
    ( Model
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


type alias Editting =
    { url : Url
    , title : Title
    }


type alias Model =
    { bookmark : Bookmark
    , editting : Maybe Editting
    }


initModel : Bookmark -> Model
initModel bookmark =
    { bookmark = bookmark
    , editting = Nothing
    }


type Msg
    = InputUrl Url
    | InputTitle Title
    | Save
    | Cancel


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.editting ) of
        ( InputUrl url, Just editting ) ->
            ( { model
                | editting = Just { editting | url = url }
              }
            , Cmd.none
            )

        ( InputTitle title, Just editting ) ->
            ( { model
                | editting = Just { editting | title = title }
              }
            , Cmd.none
            )

        ( Save, Just editting ) ->
            ( { model
                | bookmark =
                    { url = editting.url
                    , title = editting.title
                    }
                , editting = Nothing
              }
            , Cmd.none
            )

        ( Cancel, Just _ ) ->
            ( { model
                | editting = Nothing
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div
        [ class "bookmark-editor" ]
        [ inputFormView model.bookmark
        , saveButtonView
        , cancelButtonView
        ]


inputFormView : Bookmark -> Html Msg
inputFormView bookmark =
    div
        [ class "input-form" ]
        [ inputViewWrapper "title" bookmark.title InputTitle
        , inputViewWrapper "url" bookmark.url InputTitle
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
