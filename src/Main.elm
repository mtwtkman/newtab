port module Main exposing (main, move)

import Browser exposing (element)
import DnD
import Html exposing (Attribute, Html, a, button, div, i, img, input, text)
import Html.Attributes exposing (class, href, src, title, value)
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


port exportBookmarks : () -> Cmd msg



-- INIT


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        bookmarks =
            decodeBookmarks flags
    in
    ( { bookmarks = bookmarks
      , viewMode = DisplayBookmarks
      , draggable = dnd.model
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
    , draggable : DnD.Draggable Int (Int, Bookmark)
    }


dnd : DnD.DraggableInit Int (Int, Bookmark) Msg
dnd =
    DnD.init DnDMsg OnDrop


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
    | OpenEdit EditType
    | Edit EditMsg
    | Save
    | Cancel
    | Remove Int
    | FetchSource
    | GotSource (Result Http.Error (List Bookmark))
    | InputLoaderSource String
    | ExportBookmarks
    | OnDrop Int (Int, Bookmark)
    | DnDMsg (DnD.Msg Int (Int, Bookmark))


type EditMsg
    = InputUrl Url
    | InputTitle Title



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.viewMode ) of
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

        ( ExportBookmarks, _ ) ->
            ( model, exportBookmarks () )

        ( OnDrop to (from, _), DisplayBookmarks ) ->
            let
                updatedBookmarks =
                    move from to model.bookmarks
            in
            ( { model | bookmarks = updatedBookmarks }
            , updateBookmarks (encodeBookmarks updatedBookmarks)
            )

        ( DnDMsg dndmsg, DisplayBookmarks ) ->
            ( { model | draggable = DnD.update dndmsg model.draggable }, Cmd.none )

        _ ->
            ( model, Cmd.none )


move : Int -> Int -> List a -> List a
move from to xs =
    if from == to then
        xs

    else if from < to then
        let
            former =
                List.take from xs

            target =
                List.drop from xs |> List.take 1

            middle =
                List.drop (from + 1) xs |> List.take (to - from)

            latter =
                List.drop (to + 1) xs
        in
        former ++ middle ++ target ++ latter

    else
        let
            former =
                List.take to xs

            target =
                List.drop from xs |> List.take 1

            middle =
                List.drop to xs |> List.take (from - to)

            latter =
                List.drop (from + 1) xs
        in
        former ++ target ++ middle ++ latter


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
                , DnD.dragged model.draggable dragged
                ]
                    ++ (if List.isEmpty model.bookmarks then
                            []

                        else
                            [ exportBookmarksView ]
                       )

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
        (List.indexedMap bookmarkView model.bookmarks )


bookmarkView : Int -> Bookmark -> Html Msg
bookmarkView i bookmark =
    let
        node =
            div
                [ class "bookmark-item"
                ]
                [ a
                    [ href bookmark.url ]
                    [ img
                        [ defaultSizedFaviconUrl bookmark |> src
                        , title bookmark.title
                        ]
                        []
                    , text bookmark.title
                    ]
                , button
                    [ onClick (OpenEdit (KnownBookmark i bookmark)) ]
                    [ text "*" ]
                , button
                    [ onClick (Remove i) ]
                    [ text "-" ]
                ]
    in
    div
        [ class "dnd-item" ]
        [ droppable i <|
            dnd.draggable (i, bookmark) [] [ node ]
        ]


dragged : (Int, Bookmark) -> Html Msg
dragged (_, bookmark) =
  div []
    [ text bookmark.title
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


exportBookmarksButton : Html Msg
exportBookmarksButton =
    buttonViewWrapper
        [ class "export-bookmarks-button" ]
        "export"
        ExportBookmarks


exportBookmarksView : Html Msg
exportBookmarksView =
    div
        [ class "export-bookmarks-wrapper" ]
        [ exportBookmarksButton
        ]


droppable : Int -> Html Msg -> Html Msg
droppable index node =
    dnd.droppable
        index
        [ class "bookmark-droppable-zone"
        ]
        [ node ]



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ dnd.subscriptions model.draggable
        ]
