module View.BookmarkList exposing
    ( Model
    , Msg(..)
    , initModel
    , update
    , view
    )

import Entity exposing (Bookmark, encodeBookmarks)
import Func exposing (chunk, flip, inject)
import Html exposing (Attribute, Html, a, div, img, text)
import Html.Attributes exposing (class, draggable, href, src, title)
import Html.Events exposing (preventDefaultOn)
import Json.Decode as D
import Ports exposing (updateBookmarks)
import Url.Builder as UB exposing (crossOrigin)
import View.BookmarkEditor as Editor
import View.Widget exposing (infoButtonViewWrapper)


type Mode
    = Display (Maybe Grabbed)
    | Edit
        { state : Editor.Model
        , index : Int
        }


type alias Model =
    { rowLength : Int
    , bookmarks : List Bookmark
    , mode : Mode
    }


type alias Grabbed =
    { bookmark : Bookmark
    , hoveredIndex : Maybe Int
    , dropAreaBookmarks : List Bookmark
    }


initModel : Int -> List Bookmark -> Model
initModel rowLength bookmarks =
    { rowLength = rowLength
    , bookmarks = bookmarks
    , mode = Display Nothing
    }


type Msg
    = DragStart Int Bookmark
    | DragEnd
    | DragEnter Int
    | DragLeave
    | Drop
    | Remove Int
    | OpenEdit Int Bookmark
    | EditorMsg Editor.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.mode ) of
        ( DragStart sourceIndex bookmark, Display Nothing ) ->
            let
                dropAreaBookmarks =
                    List.indexedMap Tuple.pair model.bookmarks
                        |> List.filterMap
                            (\( i, b ) ->
                                if i == sourceIndex then
                                    Nothing

                                else
                                    Just b
                            )
            in
            ( { model
                | mode =
                    Display
                        (Just
                            { bookmark = bookmark
                            , hoveredIndex = Nothing
                            , dropAreaBookmarks = dropAreaBookmarks
                            }
                        )
              }
            , Cmd.none
            )

        ( DragEnd, Display (Just grabbed) ) ->
            case grabbed.hoveredIndex of
                Nothing ->
                    ( { model | mode = Display Nothing }, Cmd.none )

                Just targetIndex ->
                    let
                        newBookmarks =
                            inject grabbed.dropAreaBookmarks targetIndex grabbed.bookmark
                    in
                    ( { model
                        | bookmarks = newBookmarks
                        , mode = Display Nothing
                      }
                    , Cmd.none
                    )

        ( DragEnter hoveredIndex, Display (Just grabbed) ) ->
            ( { model
                | mode =
                    Display
                        (Just
                            { grabbed
                                | hoveredIndex = Just hoveredIndex
                            }
                        )
              }
            , Cmd.none
            )

        ( DragLeave, Display (Just grabbed) ) ->
            ( { model
                | mode =
                    Display
                        (Just
                            { grabbed
                                | hoveredIndex = Nothing
                            }
                        )
              }
            , Cmd.none
            )

        ( Drop, Display (Just grabbed) ) ->
            case grabbed.hoveredIndex of
                Just targetIndex ->
                    let
                        newBookmarks =
                            inject grabbed.dropAreaBookmarks targetIndex grabbed.bookmark
                    in
                    ( { model
                        | bookmarks = newBookmarks
                        , mode = Display Nothing
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( { model | mode = Display Nothing }
                    , Cmd.none
                    )

        ( Remove index, Display Nothing ) ->
            let
                updatedBookmarks =
                    List.indexedMap Tuple.pair model.bookmarks
                        |> List.filter (\( i, _ ) -> i /= index)
                        |> List.map Tuple.second
            in
            ( { model | bookmarks = updatedBookmarks }
            , updateBookmarks (encodeBookmarks updatedBookmarks)
            )

        ( OpenEdit index bookmark, Display Nothing ) ->
            ( { model
                | mode =
                    Edit
                        { state = Editor.initModel bookmark
                        , index = index
                        }
              }
            , Cmd.none
            )

        ( EditorMsg Editor.Save, Edit edit ) ->
            let
                newBookmarks =
                    List.take
                        edit.index
                        model.bookmarks
                        ++ (edit.state.bookmark :: List.drop (edit.index + 1) model.bookmarks)
            in
            ( { model
                | bookmarks = newBookmarks
                , mode = Display Nothing
              }
            , Cmd.none
            )

        ( EditorMsg Editor.Cancel, Edit _ ) ->
            ( model, Cmd.none )

        ( EditorMsg editorMsg, Edit edit ) ->
            let
                newState =
                    Editor.update editorMsg edit.state
            in
            ( { model
                | mode =
                    Edit
                        { edit
                            | state = Tuple.first newState
                        }
              }
            , Cmd.map EditorMsg (Tuple.second newState)
            )

        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    let
        chunked =
            List.indexedMap Tuple.pair model.bookmarks |> flip chunk model.rowLength
    in
    div
        [ class "bookmark-list"
        ]
        (List.map
            (\xs ->
                div
                    [ class "bookmark-items-row"
                    ]
                    (List.map
                        (\( i, b ) ->
                            div
                                []
                                [ droppable i
                                , itemView i b
                                ]
                        )
                        xs
                    )
            )
            chunked
        )


itemView : Int -> Bookmark -> Html Msg
itemView i bookmark =
    div
        [ class "bookmark-item"
        , draggable "true"
        , hijackOn "DragStart" (D.succeed <| DragStart i bookmark)
        ]
        [ div
            [ class "bookmark-item-card"
            ]
            [ div
                [ class "bookmark-info"
                ]
                [ a
                    [ href bookmark.url ]
                    [ img
                        [ defaultSizedFaviconUrl bookmark |> src
                        , title bookmark.title
                        , class "thumbnail"
                        ]
                        []
                    ]
                , text bookmark.title
                , infoButtonViewWrapper
                    [ class "edit-bookmark"
                    ]
                    "*"
                    (OpenEdit i bookmark)
                , infoButtonViewWrapper
                    [ class "delete-bookmark"
                    ]
                    "-"
                    (Remove i)
                ]
            ]
        ]


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


droppable : Int -> Html Msg
droppable index =
    div
        [ class "droppable"
        , hijackOn "dragenter" (D.succeed <| DragEnter index)
        , hijackOn "dragleave" (D.succeed DragLeave)
        , hijackOn "drop" (D.succeed <| Drop)
        ]
        []


hijack : msg -> ( msg, Bool )
hijack msg =
    ( msg, True )


hijackOn : String -> D.Decoder msg -> Attribute msg
hijackOn event decoder =
    preventDefaultOn event (D.map hijack decoder)
