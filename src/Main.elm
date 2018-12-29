module Main exposing (init, main)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Html
import ListSelect
import Url exposing (Url)


type alias Flags = { }


type alias Model =
    { page : Page
    }


type Msg
    = OnUrlChange Url
    | OnUrlRequest UrlRequest
    | OnListSelect ListSelect.Msg


type Page
  = ListSelect ListSelect.Model


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url navKey =
    ( { page = ListSelect ListSelect.init }, Cmd.none )


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }


subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    (model, Cmd.none)


view : Model -> Browser.Document Msg
view model =
    case model.page of
        ListSelect listSelectModel ->
            (ListSelect.view >> mapMsg OnListSelect) listSelectModel


mapMsg : (a -> b) -> Browser.Document a -> Browser.Document b
mapMsg f a =
    { title = a.title
    , body = List.map (Html.map f) a.body
    }
