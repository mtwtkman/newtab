module View.Loader exposing (Model(..), Msg(..), initModel, update, view)

import Entity exposing (Bookmark, bookmarksDecoder, encodeBookmarks)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Http
import Ports exposing (updateBookmarks)
import View.CancelButton exposing (cancelButtonView)
import View.Widget exposing (infoButtonViewWrapper, inputViewWrapper)


type Model
    = Input String
    | Fetched (List Bookmark)
    | Canceled


type Msg
    = InputUrl String
    | FetchSource
    | GotSource (Result Http.Error (List Bookmark))
    | Cancel


initModel : Model
initModel =
    Input ""


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( InputUrl url, Input _ ) ->
            ( Input url, Cmd.none )

        ( FetchSource, Input url ) ->
            ( model
            , Http.get
                { url = url
                , expect = Http.expectJson GotSource bookmarksDecoder
                }
            )

        ( GotSource result, Input _ ) ->
            case result of
                Err _ ->
                    ( model, Cmd.none )

                Ok bookmarks ->
                    ( Fetched bookmarks
                    , updateBookmarks (encodeBookmarks bookmarks)
                    )
        ( Cancel, _) ->
          ( Canceled, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case model of
        Input v ->
            div
                [ class "loader-wrapper" ]
                [ inputViewWrapper "loader" v InputUrl
                , infoButtonViewWrapper
                    [ class "fetch-source" ]
                    "fetch"
                    FetchSource
                , cancelButtonView Cancel
                ]

        _ ->
            div [] []
