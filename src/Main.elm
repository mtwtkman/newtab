port module Main exposing (main)

import Browser exposing (element)
import Html exposing (Html, a, button, div, img, input, text)
import Html.Attributes exposing (class, href, src, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as D
import Url.Builder as UB exposing (crossOrigin)



-- MAIN


main : Program Flags Model Msg
main =
    element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- PORTS


port addBookmark : ( Url, Title ) -> Cmd msg


port messageReceiver : (Flags -> msg) -> Sub msg



-- INIT


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        bookmarks =
            case D.decodeValue bookmarksDecoder flags of
                Err _ ->
                    []

                Ok x ->
                    x
    in
    ( { bookmarks = bookmarks, viewMode = DisplayBookmarks }, Cmd.none )



-- DECODER


bookmarkDecoder : D.Decoder Bookmark
bookmarkDecoder =
    D.map2
        Bookmark
        (D.field "url" D.string)
        (D.field "title" D.string)


bookmarksDecoder : D.Decoder (List Bookmark)
bookmarksDecoder =
    D.list bookmarkDecoder



-- MODEL


type alias Url =
    String


type alias Title =
    String


type alias Bookmark =
    { url : Url
    , title : Title
    }


newBookmark : Bookmark
newBookmark =
    { url = ""
    , title = ""
    }


type alias Flags =
    D.Value


flip : (a -> b -> c) -> b -> a -> c
flip f b a =
    f a b


faviconUrl : Bookmark -> Int -> String
faviconUrl bookmark size =
    crossOrigin "https://t2.gstatic.com"
        [ "faviconV2" ]
        [ UB.string "client" "SOCIAL"
        , UB.string "type" "FAVICON"
        , UB.string "fallback_opts" "TYPE,SIZE,URL"
        , UB.string "url" bookmark.url
        , UB.int "size" size
        ]


defaultSizedFaviconUrl : Bookmark -> String
defaultSizedFaviconUrl =
    flip faviconUrl 32


type alias Model =
    { bookmarks : List Bookmark
    , viewMode : ViewMode
    }


type EditType
    = NewBookmark
    | KnownBookmark Int


type ViewMode
    = DisplayBookmarks
    | EditBookmark Bookmark EditType



-- MESSAGE


type Msg
    = LoadBookmarks
    | ReadDefaultBookmarks String
    | ReceiveLatestBookmarks Flags
    | OpenEdit EditType
    | Edit EditMsg
    | Save
    | Cancel
    | Remove Int


type EditMsg
    = InputUrl Url
    | InputTitle Title



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.viewMode ) of
        ( OpenEdit NewBookmark, DisplayBookmarks ) ->
            ( { model | viewMode = EditBookmark newBookmark NewBookmark }, Cmd.none )

        ( Edit editMsg, EditBookmark bookmark editType ) ->
            let
                newViewMode =
                    EditBookmark (updateEditingBookmark editMsg bookmark) editType
            in
            ( { model | viewMode = newViewMode }, Cmd.none )

        ( Save, EditBookmark bookmark NewBookmark ) ->
            ( { model
                | viewMode = DisplayBookmarks
                , bookmarks = model.bookmarks ++ [ bookmark ]
              }
            , addBookmark ( bookmark.url, bookmark.title )
            )
        _ ->
            ( model, Cmd.none )


updateEditingBookmark : EditMsg -> Bookmark -> Bookmark
updateEditingBookmark msg bookmark =
    case msg of
        InputUrl url ->
            { bookmark | url = url }

        InputTitle title ->
            { bookmark | title = title }



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ class "toplevel" ]
        (case model.viewMode of
            DisplayBookmarks ->
                [ bookmarkListView model
                , newBookmarkAddButtonView
                ]

            EditBookmark bookmark NewBookmark ->
                [ bookmarkEditorView bookmark
                ]

            _ ->
                []
        )


newBookmarkAddButtonView : Html Msg
newBookmarkAddButtonView =
    button
        [ onClick (OpenEdit NewBookmark)
        , class "new-bookmark-add-button"
        ]
        [ text "+" ]


inputView : String -> String -> (String -> EditMsg) -> Html EditMsg
inputView label inputValue handler =
    div
        [ class ("input-" ++ label) ]
        [ input
            [ value inputValue
            , onInput handler
            ]
            []
        ]


inputFormView : Bookmark -> Html Msg
inputFormView bookmark =
    div
        [ class "input-form" ]
        [ Html.map Edit (inputView "title" bookmark.title InputTitle)
        , Html.map Edit (inputView "url" bookmark.url InputUrl)
        ]


saveButtonView : Html Msg
saveButtonView =
    div
        [ class "save" ]
        [ button
            [ onClick Save ]
            [ text "save" ]
        ]


cancelButtonView : Html Msg
cancelButtonView =
    div
        [ class "cancel" ]
        [ button
            [ onClick Cancel ]
            [ text "cancel" ]
        ]


bookmarkEditorView : Bookmark -> Html Msg
bookmarkEditorView bookmark =
    div
        [ class "bookmark-editor" ]
        [ inputFormView bookmark
        , saveButtonView
        , cancelButtonView
        ]


bookmarkListView : Model -> Html Msg
bookmarkListView model =
    div
        [ class "bookmark-list" ]
        (List.map bookmarkView model.bookmarks)


bookmarkView : Bookmark -> Html Msg
bookmarkView bookmark =
    div [ class "bookmark-item" ]
        [ a
            [ href bookmark.url ]
            [ img [ defaultSizedFaviconUrl bookmark |> src ] []
            , text bookmark.title
            ]
        ]



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions _ =
    messageReceiver ReceiveLatestBookmarks
