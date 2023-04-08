module Main exposing (main)

import Browser exposing (element)
import Entity exposing (Bookmark, decodeBookmarks, encodeBookmarks, newBookmark)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Json.Decode as D
import Ports exposing (exportBookmarks, updateBookmarks)
import View.BookmarkEditor as Editor
import View.BookmarkList as BL
import View.Export exposing (exportBookmarksView)
import View.Loader as Loader exposing (Model)
import View.Widget exposing (infoButtonViewWrapper)



-- MAIN


main : Program Flags Model Msg
main =
    element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type ViewMode
    = DisplayBookmarks BL.Model
    | SourceLoader Loader.Model
    | AddNewBookmark Editor.Model


rowLength : Int
rowLength =
    8


initBookmarkList : List Bookmark -> BL.Model
initBookmarkList =
    BL.initModel rowLength



-- INIT


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        bookmarks =
            decodeBookmarks flags
    in
    ( { bookmarks = bookmarks
      , viewMode = DisplayBookmarks (initBookmarkList bookmarks)
      }
    , Cmd.none
    )



-- MODEL


type alias Flags =
    D.Value


type alias Model =
    { bookmarks : List Bookmark
    , viewMode : ViewMode
    }



-- MESSAGE


type Msg
    = OpenLoader
    | ExportBookmarks
    | OpenNewBookmarkEditor
    | GotAddNewBookmarkMsg Editor.Msg
    | GotLoaderMsg Loader.Msg
    | GotBookmarkListMsg BL.Msg



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.viewMode ) of
        ( OpenLoader, DisplayBookmarks _ ) ->
            ( { model | viewMode = SourceLoader Loader.initModel }, Cmd.none )

        ( ExportBookmarks, DisplayBookmarks _ ) ->
            ( model, exportBookmarks () )

        ( OpenNewBookmarkEditor, DisplayBookmarks _ ) ->
            ( { model | viewMode = AddNewBookmark (Editor.initModel newBookmark) }, Cmd.none )

        ( GotBookmarkListMsg bookmarkListMsg, DisplayBookmarks bookmarkListModel ) ->
            let
                ( em, ec ) =
                    BL.update bookmarkListMsg bookmarkListModel
            in
            ( { model
                | bookmarks = em.bookmarks
                , viewMode = DisplayBookmarks em
              }
            , Cmd.map GotBookmarkListMsg ec
            )

        ( GotAddNewBookmarkMsg editorMsg, AddNewBookmark editorModel ) ->
            let
                ( em, ec ) =
                    Editor.update editorMsg editorModel
            in
            case em of
                Editor.Saved newBookmark ->
                    let
                        newBookmarks =
                            model.bookmarks ++ [ newBookmark ]
                    in
                    ( { model
                        | viewMode = DisplayBookmarks (initBookmarkList newBookmarks)
                      }
                    , updateBookmarks (encodeBookmarks newBookmarks)
                    )

                Editor.Canceled ->
                    ( { model
                        | viewMode = DisplayBookmarks (initBookmarkList model.bookmarks)
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model
                        | viewMode = AddNewBookmark em
                      }
                    , Cmd.map GotAddNewBookmarkMsg ec
                    )

        ( GotLoaderMsg loaderMsg, SourceLoader loaderModel ) ->
            let
                ( em, ec ) =
                    Loader.update loaderMsg loaderModel
            in
            case em of
                Loader.Canceled ->
                    ( { model
                        | viewMode = DisplayBookmarks (initBookmarkList model.bookmarks)
                      }
                    , Cmd.none
                    )

                Loader.Fetched newBookmarks ->
                    ( { model
                        | viewMode = DisplayBookmarks (initBookmarkList newBookmarks)
                      }
                    , Cmd.map GotLoaderMsg ec
                    )

                _ ->
                    ( { model
                        | viewMode = SourceLoader em
                      }
                    , Cmd.map GotLoaderMsg ec
                    )

        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ class "toplevel"
        ]
        (case model.viewMode of
            DisplayBookmarks bookmarkListModel ->
                [ div
                    [ class "widgets"
                    ]
                    ([ newBookmarkAddButtonView
                     , loaderSettingButtonView
                     ]
                        ++ (if List.isEmpty model.bookmarks then
                                []

                            else
                                [ exportBookmarksView ExportBookmarks ]
                           )
                    )
                , Html.map GotBookmarkListMsg (BL.view bookmarkListModel)
                ]

            AddNewBookmark editorModel ->
                [ Html.map GotAddNewBookmarkMsg (Editor.view editorModel)
                ]

            SourceLoader loaderModel ->
                [ Html.map GotLoaderMsg (Loader.view loaderModel)
                ]
        )


newBookmarkAddButtonView : Html Msg
newBookmarkAddButtonView =
    infoButtonViewWrapper
        [ class "new-bookmark-add-button"
        ]
        "+"
        OpenNewBookmarkEditor


loaderSettingButtonView : Html Msg
loaderSettingButtonView =
    infoButtonViewWrapper
        [ class "loader-setting" ]
        "load"
        OpenLoader



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions =
    always Sub.none
