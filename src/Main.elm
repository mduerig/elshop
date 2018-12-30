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
    | OnListSelectChange ListSelect.Model
    | OnListSelect ListSelect.ShoppingList


type Page
  = ListSelect ListSelect.Model

listSelectConfig : ListSelect.Config Msg
listSelectConfig =
    { onChange = OnListSelectChange
    , onSelect = OnListSelect
    }

init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url navKey =
    ( { page = ListSelect ListSelect.init }
    , Cmd.none
    )


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
    case msg of
        OnUrlChange url ->
            ( model, Cmd.none )

        OnUrlRequest urlRequest ->
            ( model, Cmd.none )

        OnListSelectChange listSelectModel ->
            ( { model | page = ListSelect listSelectModel }
            , Cmd.none
            )

        OnListSelect shoppingList ->
            ( Debug.log ("select" ++ (Debug.toString shoppingList)) model
            , Cmd.none
            )


view : Model -> Browser.Document Msg
view model =
    case model.page of
        ListSelect listSelectModel ->
            ListSelect.view listSelectConfig listSelectModel
