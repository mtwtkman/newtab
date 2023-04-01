module View.BookmarkList exposing (bookmarkListView, dragged)

import DnD
import Entity exposing (Bookmark)
import Func exposing (chunk, flip)
import Html exposing (Html, a, button, div, img, text)
import Html.Attributes exposing (class, href, src, title)
import Html.Events exposing (onClick)
import Url.Builder as UB exposing (crossOrigin)
import Viewmode exposing (EditType(..))


bookmarkListView :
    Int
    -> DnD.DraggableInit Int Bookmark msg
    -> DnD.Draggable Int Bookmark
    -> (EditType -> msg)
    -> (Int -> msg)
    -> List Bookmark
    -> Html msg
bookmarkListView rowLength dnd dndModel openHandler removeHandler bookmarks =
    let
        chunked =
            List.indexedMap Tuple.pair bookmarks |> flip chunk rowLength
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
                            bookmarkView
                                dnd
                                dndModel
                                openHandler
                                removeHandler
                                i
                                b
                        )
                        xs
                    )
            )
            chunked
        )


bookmarkView :
    DnD.DraggableInit Int Bookmark msg
    -> DnD.Draggable Int Bookmark
    -> (EditType -> msg)
    -> (Int -> msg)
    -> Int
    -> Bookmark
    -> Html msg
bookmarkView dnd dndModel openHandler removeHandler i bookmark =
    div
        [ class "bookmark-item"
        ]
        [ div
            [ class "bookmark-dnd-area"
            ]
            [ droppable dnd dndModel i
            , dnd.draggable bookmark
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
                            ]
                            []
                        ]
                    , text bookmark.title
                    , button
                        [ onClick (openHandler (KnownBookmark i bookmark)) ]
                        [ text "*" ]
                    , button
                        [ onClick (removeHandler i) ]
                        [ text "-" ]
                    ]
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


dragged : Bookmark -> Html msg
dragged bookmark =
    div []
        [ text bookmark.title
        ]


droppable :
    DnD.DraggableInit Int Bookmark msg
    -> DnD.Draggable Int Bookmark
    -> Int
    -> Html msg
droppable dnd dndModel index =
    let
        is_on =
            case DnD.getDropMeta dndModel of
                Just to ->
                    to == index

                _ ->
                    False
    in
    dnd.droppable
        index
        [ class "bookmark-droppable-zone"
        , class "column"
        , class "is-1"
        , if is_on then
            class "drag-over"

          else
            class "non-touched"
        ]
        (if is_on
          then
            [ text "|" ]
          else
            []
        )
