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
import Html.Events exposing (on, preventDefaultOn)
import Json.Decode as D
import Ports exposing (updateBookmarks)
import Url.Builder as UB exposing (crossOrigin)
import View.BookmarkEditor as Editor
import View.Widget exposing (infoButtonViewWrapper)


type alias Model =
    { rowLength : Int
    , bookmarks : List Bookmark
    , mode : Mode
    }


type Mode
    = Display (Maybe Grabbed)
    | Edit Int Editor.Model


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
    | DragLeave
    | DragOver Int
    | Drop
    | Remove Int
    | OpenEdit Int Bookmark
    | GotEditorMsg Editor.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.mode ) of
        ( DragOver index, Display (Just grabbed) ) ->
            ( { model
                | mode = Display (Just { grabbed | hoveredIndex = Just index })
              }
            , Cmd.none
            )

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
                    , updateBookmarks (encodeBookmarks newBookmarks)
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
                    Edit index (Editor.initModel bookmark)
              }
            , Cmd.none
            )

        ( GotEditorMsg editorMsg, Edit index editorModel ) ->
            let
                ( m, c ) =
                    Editor.update editorMsg editorModel
            in
            case m of
                Editor.Saved updatedBookmark ->
                    let
                        newBookmarks =
                            List.take
                                index
                                model.bookmarks
                                ++ (updatedBookmark :: List.drop (index + 1) model.bookmarks)
                    in
                    ( { model
                        | bookmarks = newBookmarks
                        , mode = Display Nothing
                      }
                    , Cmd.map GotEditorMsg c
                    )

                Editor.Canceled ->
                    ( { model | mode = Display Nothing }, Cmd.none )

                _ ->
                    ( { model | mode = Edit index m }, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    let
        chunked =
            List.indexedMap Tuple.pair model.bookmarks |> flip chunk model.rowLength
    in
    case model.mode of
        Display grabbed ->
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
                                        [ class "bookmark-items-cell"
                                        ]
                                        [ droppable i grabbed
                                        , itemView i b
                                        ]
                                )
                                xs
                            )
                    )
                    chunked
                )

        Edit _ editorModel ->
            Html.map GotEditorMsg (Editor.view editorModel)


itemView : Int -> Bookmark -> Html Msg
itemView i bookmark =
    div
        [ class "bookmark-item"
        , draggable "true"
        , on "dragstart" (D.succeed <| DragStart i bookmark)
        , on "dragend" (D.succeed DragEnd)
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


droppable : Int -> Maybe Grabbed -> Html Msg
droppable index maybeGrabbed =
    div
        [ class "droppable"
        , on "dragleave" (D.succeed DragLeave)
        , on "drop" (D.succeed <| Drop)
        , hijackOn "dragover" (D.succeed <| DragOver index)
        , class <|
            case maybeGrabbed of
                Just grabbed ->
                    Maybe.withDefault ""
                        (Maybe.andThen
                            (\x ->
                                Just <|
                                    if x == index then
                                        "over-dropzone"

                                    else
                                        ""
                            )
                            grabbed.hoveredIndex
                        )

                Nothing ->
                    ""
        ]
        []


hijack : msg -> ( msg, Bool )
hijack msg =
    ( msg, True )


hijackOn : String -> D.Decoder msg -> Attribute msg
hijackOn event decoder =
    preventDefaultOn event (D.map hijack decoder)
