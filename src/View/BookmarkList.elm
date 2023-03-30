module View.BookmarkList exposing (bookmarkListView, dragged)

import DnD
import Entity exposing (Bookmark)
import Func exposing (flip)
import Html exposing (Html, a, button, div, img, text)
import Html.Attributes exposing (class, href, src, title)
import Html.Events exposing (onClick)
import Url.Builder as UB exposing (crossOrigin)
import Viewmode exposing (EditType(..))


bookmarkListView : Int -> DnD.DraggableInit Int Bookmark msg -> (EditType -> msg) -> (Int -> msg) -> List Bookmark -> Html msg
bookmarkListView rowLength dnd openHandler removeHandler bookmarks =
    div
        [ class "bookmark-list"
        ]
        (List.indexedMap
            (\i b ->
                bookmarkView
                    dnd
                    openHandler
                    removeHandler
                    i
                    b
            )
            bookmarks
        )


bookmarkView : DnD.DraggableInit Int Bookmark msg -> (EditType -> msg) -> (Int -> msg) -> Int -> Bookmark -> Html msg
bookmarkView dnd openHandler removeHandler i bookmark =
    div
        [ class "dnd-item" ]
        [ droppable dnd i
        , dnd.draggable bookmark
            []
            [ div
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
                    [ onClick (openHandler (KnownBookmark i bookmark)) ]
                    [ text "*" ]
                , button
                    [ onClick (removeHandler i) ]
                    [ text "-" ]
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


droppable : DnD.DraggableInit Int Bookmark msg -> Int -> Html msg
droppable dnd index =
    dnd.droppable
        index
        [ class "bookmark-droppable-zone"
        ]
        []
