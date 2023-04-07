module Entity exposing
    ( Bookmark
    , Title
    , Url
    , bookmarksDecoder
    , decodeBookmarks
    , encodeBookmarks
    , newBookmark
    )

import Json.Decode as D
import Json.Encode as E


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


decodeBookmarks : D.Value -> List Bookmark
decodeBookmarks flags =
    case D.decodeValue bookmarksDecoder flags of
        Err _ ->
            []

        Ok x ->
            x
