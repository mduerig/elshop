module Main exposing (init, main)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import ListEdit
import ListSelect
import ShoppingList exposing (ShoppingList)
import Url exposing (Url)


type alias Flags = { }


type alias Model =
    { lists : List ShoppingList
    , page : Page
    }


type Msg
    = OnUrlChange Url
    | OnUrlRequest UrlRequest
    | OnListSelectChange ListSelect.Model
    | OnListSelect (List ShoppingList) ShoppingList
    | OnListChange ListEdit.Model
    | OnListEditExit ShoppingList


type Page
  = ListSelect ListSelect.Model
  | ListEdit ListEdit.Model


listSelectConfig : ListSelect.Config Msg
listSelectConfig =
    { onChange = OnListSelectChange
    , onSelect = OnListSelect
    }


listEditConfig : ListEdit.Config Msg
listEditConfig =
    { onChange = OnListChange
    , onExit = OnListEditExit
    }


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        lists =
            [ ShoppingList.addItem (ShoppingList.newList "list one")  "foo"
            , ShoppingList.newList "list two"
            ]
        model =
            { lists = lists
            , page = ListSelect (ListSelect.init lists)
            }
    in
        ( model , Cmd.none )


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
            ( { model
                | page = ListSelect listSelectModel }
            , Cmd.none
            )

        OnListSelect lists shoppingList ->
            ( { model
                | lists = lists
                , page = ListEdit (ListEdit.init shoppingList) }
            , Cmd.none
            )

        OnListChange listModel ->
            ( { model
                | page = ListEdit listModel }
            , Cmd.none
            )

        OnListEditExit shoppingList ->
            let
                updatedLists = updateLists model.lists shoppingList
            in
                ( { model
                    | lists = updatedLists
                    , page = ListSelect (ListSelect.init updatedLists)
                    }
                , Cmd.none
                )


updateLists : List ShoppingList -> ShoppingList -> List ShoppingList
updateLists lists updatedList =
    let
        updateList list =
            if list.name == updatedList.name then
                updatedList
            else
                list
    in
        List.map (updateList) lists


view : Model -> Browser.Document Msg
view model =
    case model.page of
        ListSelect listSelectModel ->
            ListSelect.view listSelectConfig listSelectModel

        ListEdit modelEditModel ->
            ListEdit.view listEditConfig modelEditModel
