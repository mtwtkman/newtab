port module Main exposing (main, move)

import Browser exposing (element)
import DnD
import Entity exposing (Bookmark, Title, Url, newBookmark)
import Html exposing (Html, button, div, i, text)
import Html.Attributes exposing (class, title)
import Html.Events exposing (onClick)
import Http
import Json.Decode as D
import Json.Encode as E
import View.BookmarkEditor exposing (bookmarkEditorView)
import View.BookmarkList exposing (bookmarkListView, dragged)
import View.Export exposing (exportBookmarksView)
import View.Loader exposing (loaderSettingButtonView, loaderView)
import Viewmode exposing (EditType(..), ViewMode(..))



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
      , draggingBookmarks = []
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


type alias Flags =
    D.Value


type alias Model =
    { bookmarks : List Bookmark
    , viewMode : ViewMode
    , draggable : DnD.Draggable Int ( Int, Bookmark )
    , draggingBookmarks : List Bookmark
    }


dnd : DnD.DraggableInit Int ( Int, Bookmark ) Msg
dnd =
    DnD.init DnDMsg OnDrop



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
    | OnDrop Int ( Int, Bookmark )
    | DnDMsg (DnD.Msg Int ( Int, Bookmark ))


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

        ( OnDrop to ( from, _ ), DisplayBookmarks ) ->
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
                [ bookmarkListView dnd OpenEdit Remove model.bookmarks
                , newBookmarkAddButtonView
                , loaderSettingButtonView LoadBookmarks
                , DnD.dragged model.draggable dragged
                ]
                    ++ (if List.isEmpty model.bookmarks then
                            []

                        else
                            [ exportBookmarksView ExportBookmarks ]
                       )

            EditBookmark bookmark NewBookmark ->
                [ bookmarkEditorView
                    Edit
                    InputTitle
                    InputUrl
                    Save
                    Cancel
                    bookmark
                ]

            EditBookmark bookmark (KnownBookmark _ _) ->
                [ bookmarkEditorView
                    Edit
                    InputTitle
                    InputUrl
                    Save
                    Cancel
                    bookmark
                ]

            Loader _ ->
                [ loaderView InputLoaderSource FetchSource
                ]
        )


newBookmarkAddButtonView : Html Msg
newBookmarkAddButtonView =
    button
        [ onClick (OpenEdit NewBookmark)
        , class "new-bookmark-add-button"
        ]
        [ text "+" ]



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ dnd.subscriptions model.draggable
        ]
