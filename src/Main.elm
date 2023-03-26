port module Main exposing (main)

import Browser exposing (element)
import Html exposing (Attribute, Html, a, button, div, img, input, text)
import Html.Attributes exposing (class, href, src, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as D
import Json.Encode as E
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


port updateBookmarks : E.Value -> Cmd msg


port messageReceiver : (Flags -> msg) -> Sub msg



-- INIT


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        bookmarks =
            decodeBookmarks flags
    in
    ( { bookmarks = bookmarks
      , viewMode = DisplayBookmarks
      }
    , Cmd.none
    )



-- DECODER


decodeBookmarks : Flags -> List Bookmark
decodeBookmarks flags =
    case D.decodeValue bookmarksDecoder flags of
        Err _ ->
            []

        Ok x ->
            x


bookmarkDecoder : D.Decoder Bookmark
bookmarkDecoder =
    D.map2
        Bookmark
        (D.field "url" D.string)
        (D.field "title" D.string)


bookmarksDecoder : D.Decoder (List Bookmark)
bookmarksDecoder =
    D.list bookmarkDecoder



-- ENCODER


encodeBookmark : Bookmark -> E.Value
encodeBookmark bookmark =
    E.object
        [ ( "url", E.string bookmark.url )
        , ( "title", E.string bookmark.title )
        ]


encodeBookmarks : List Bookmark -> E.Value
encodeBookmarks bookmarks =
    E.list encodeBookmark bookmarks



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
    | KnownBookmark Int Bookmark


type ViewMode
    = DisplayBookmarks
    | EditBookmark Bookmark EditType
    | Loader String



-- MESSAGE


type Msg
    = LoadBookmarks
    | ReceiveLatestBookmarks Flags
    | OpenEdit EditType
    | Edit EditMsg
    | Save
    | Cancel
    | Remove Int
    | FetchSource
    | GotSource (Result Http.Error (List Bookmark))
    | InputLoaderSource String


type EditMsg
    = InputUrl Url
    | InputTitle Title



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.viewMode ) of
        ( ReceiveLatestBookmarks flags, _ ) ->
            ( { model | bookmarks = decodeBookmarks flags }, Cmd.none )

        ( OpenEdit NewBookmark, DisplayBookmarks ) ->
            ( { model | viewMode = EditBookmark newBookmark NewBookmark }, Cmd.none )

        ( OpenEdit (KnownBookmark i bookmark), DisplayBookmarks ) ->
            ( { model | viewMode = EditBookmark bookmark (KnownBookmark i bookmark) }, Cmd.none )

        ( Edit editMsg, EditBookmark bookmark editType ) ->
            let
                newViewMode =
                    EditBookmark (updateEditingBookmark editMsg bookmark) editType
            in
            ( { model | viewMode = newViewMode }, Cmd.none )

        ( Save, EditBookmark bookmark NewBookmark ) ->
            let
                updatedBookmarks =
                    model.bookmarks ++ [ bookmark ]
            in
            ( { model
                | viewMode = DisplayBookmarks
                , bookmarks = updatedBookmarks
              }
            , updateBookmarks (encodeBookmarks updatedBookmarks)
            )

        ( Save, EditBookmark bookmark (KnownBookmark index _) ) ->
            let
                updatedBookmarks =
                    List.take index model.bookmarks ++ (bookmark :: List.drop (index + 1) model.bookmarks)
            in
            ( { model
                | viewMode = DisplayBookmarks
                , bookmarks = updatedBookmarks
              }
            , updateBookmarks (encodeBookmarks updatedBookmarks)
            )

        ( Remove index, _ ) ->
            let
                updatedBookmarks =
                    List.indexedMap Tuple.pair model.bookmarks
                        |> List.filter (\( i, _ ) -> i /= index)
                        |> List.map Tuple.second
            in
            ( { model | bookmarks = updatedBookmarks }
            , updateBookmarks (encodeBookmarks updatedBookmarks)
            )

        ( Cancel, _ ) ->
            ( { model | viewMode = DisplayBookmarks }
            , Cmd.none
            )

        ( LoadBookmarks, DisplayBookmarks ) ->
            ( { model | viewMode = Loader "" }
            , Cmd.none
            )

        ( InputLoaderSource val, Loader v ) ->
            ( { model | viewMode = Loader (v ++ val) }
            , Cmd.none
            )

        ( FetchSource, Loader url ) ->
            ( model
            , Http.get
                { url = url
                , expect = Http.expectJson GotSource bookmarksDecoder
                }
            )

        ( GotSource result, _ ) ->
            case result of
                Err _ ->
                    ( { model | viewMode = DisplayBookmarks }, Cmd.none )

                Ok bookmarks ->
                    ( { model
                        | bookmarks = bookmarks
                        , viewMode = DisplayBookmarks
                      }
                    , updateBookmarks (encodeBookmarks bookmarks)
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
                , loaderSettingButtonView
                ]

            EditBookmark bookmark NewBookmark ->
                [ bookmarkEditorView bookmark
                ]

            EditBookmark bookmark (KnownBookmark _ _) ->
                [ bookmarkEditorView bookmark
                ]

            Loader _ ->
                [ loaderView
                ]
        )


newBookmarkAddButtonView : Html Msg
newBookmarkAddButtonView =
    button
        [ onClick (OpenEdit NewBookmark)
        , class "new-bookmark-add-button"
        ]
        [ text "+" ]


buttonViewWrapper : List (Attribute msg) -> String -> msg -> Html msg
buttonViewWrapper attrs label msg =
    div
        attrs
        [ button
            [ onClick msg ]
            [ text label ]
        ]


loaderSettingButtonView : Html Msg
loaderSettingButtonView =
    buttonViewWrapper
        [ class "loader-setting" ]
        "load"
        LoadBookmarks


inputViewWrapper : String -> String -> (String -> EditMsg) -> Html EditMsg
inputViewWrapper label inputValue handler =
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
        [ Html.map Edit (inputViewWrapper "title" bookmark.title InputTitle)
        , Html.map Edit (inputViewWrapper "url" bookmark.url InputUrl)
        ]


saveButtonView : Html Msg
saveButtonView =
    buttonViewWrapper
        [ class "save" ]
        "save"
        Save


cancelButtonView : Html Msg
cancelButtonView =
    buttonViewWrapper
        [ class "cancel" ]
        "cancel"
        Cancel


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
        (List.indexedMap bookmarkView model.bookmarks)


bookmarkView : Int -> Bookmark -> Html Msg
bookmarkView i bookmark =
    div [ class "bookmark-item" ]
        [ a
            [ href bookmark.url ]
            [ img [ defaultSizedFaviconUrl bookmark |> src ] []
            , text bookmark.title
            ]
        , button
            [ onClick (OpenEdit (KnownBookmark i bookmark)) ]
            [ text "*" ]
        , button
            [ onClick (Remove i) ]
            [ text "-" ]
        ]


loaderView : Html Msg
loaderView =
    div
        [ class "loader-wrapper" ]
        [ input
            [ onInput InputLoaderSource ]
            []
        , buttonViewWrapper
            [ class "fetch-source" ]
            "fetch"
            FetchSource
        ]



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions _ =
    messageReceiver ReceiveLatestBookmarks
