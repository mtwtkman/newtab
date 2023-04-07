port module Ports exposing (exportBookmarks, updateBookmarks)

import Json.Encode as E


port updateBookmarks : E.Value -> Cmd msg


port exportBookmarks : () -> Cmd msg
