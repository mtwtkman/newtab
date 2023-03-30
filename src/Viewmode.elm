module Viewmode exposing (EditType(..), ViewMode(..))

import Entity exposing (Bookmark)


type EditType
    = NewBookmark
    | KnownBookmark Int Bookmark


type ViewMode
    = DisplayBookmarks
    | EditBookmark Bookmark EditType
    | Loader String
