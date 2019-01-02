module ListSelect exposing (Config, Model, init, view)

import Browser
import Html exposing (Html, a, div, h1, text)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import ShoppingList exposing (ShoppingList)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Utilities.Border as Border
import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.Form.Input as Input
import Bootstrap.Grid.Col as Col
import Bootstrap.Modal as Modal
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Badge as Badge
import Bootstrap.Utilities.Flex as Flex


type alias Model =
    { lists : List ShoppingList
    , newList : Maybe String
    , error : Maybe String
    , modal : Modal.Visibility
    }


type alias Config msg =
    { onChange : Model -> msg
    , onSelect : (List ShoppingList) -> ShoppingList -> msg
    }


init : List ShoppingList -> Model
init lists =
    { lists = lists
    , newList = Nothing
    , error = Nothing
    , modal = Modal.hidden
    }


view : Config msg -> Model -> Browser.Document msg
view config model =
    { title = "Elm Shopping"
    , body =
        [ CDN.stylesheet
        , Grid.container
            [ Border.all, Border.rounded ]
            [ h1 [] [ text "Elm Shopping" ]
            , text "Choose or create shopping list"
            , existingLists config model
            , newListButton config model
            , newListModalDialog config model
            ]
        ]
    }


existingLists : Config msg -> Model -> Html msg
existingLists config model =
    let
        toListItem list =
            ListGroup.anchor
                [ ListGroup.attrs
                     [ onClick <| config.onSelect model.lists list
                     , Flex.block, Flex.justifyBetween, Flex.alignItemsCenter
                     ]
                ]
                [ a [ href "" ] [ text list.name ]
                , Badge.pillInfo [] [ listInfo list ]
                ]
    in
        ListGroup.custom
            ( model.lists
                |> List.reverse
                >> List.map toListItem
            )


listInfo : ShoppingList -> Html msg
listInfo shoppingList =
    let
        totalItems =
            shoppingList
                |> ShoppingList.checkedItemCount
                |> String.fromInt

        checkedItems =
            shoppingList
                |> ShoppingList.itemCount
                |> String.fromInt
    in
        text (totalItems ++ " / " ++ checkedItems)

newListButton : Config msg -> Model -> Html msg
newListButton config model =
    Button.button
        [ Button.onClick <| config.onChange
            { model | modal = Modal.shown }
        ]
        [ text "Create a Shopping list"]


newListModalDialog : Config msg -> Model -> Html msg
newListModalDialog config model =
    Modal.config (cancelModal config model)
        |> Modal.large
        |> Modal.hideOnBackdropClick False
        |> Modal.h3 [] [ text "New Shopping List" ]
        |> Modal.body []
            [ newListTextBox config model
            , showError model
            ]
        |> Modal.footer []
            [ Button.button
                [ Button.outlinePrimary
                , Button.attrs [ onClick (cancelModal config model) ]
                ]
                [ text "Cancel" ]
            , okButton config model
            ]
        |> Modal.view model.modal


newListTextBox : Config msg -> Model -> Html msg
newListTextBox config model =
    let
        danger =
            if model.error /= Nothing then
                [ Input.danger ]
            else
                []
    in
        Grid.row []
            [ Grid.col [ ]
                [ Input.text (
                    [ Input.value (Maybe.withDefault "" model.newList)
                    , onNewListName config model
                    ] ++ danger )
                ]
            ]


cancelModal : Config msg -> Model -> msg
cancelModal config model
    = config.onChange { model | modal = Modal.hidden }


okButton : Config msg -> Model -> Html msg
okButton config model =
    let
        disable
            =  model.error /= Nothing
            || model.newList == Nothing
            || model.newList == Just ""
    in
        Button.button
            [ Button.primary
            , Button.disabled disable
            , onNewListCreate config model
            ]
            [ text "Create" ]


onNewListCreate : Config msg -> Model -> Button.Option msg
onNewListCreate config model =
    let
        (updatedLists, newInput) =
            case model.newList of
                Just name ->
                    if listExists name model.lists then
                        (model.lists, Just name)
                    else
                        (ShoppingList.newList name :: model.lists, Nothing)

                Nothing ->
                    (model.lists, Nothing)
    in
        Button.onClick <| config.onChange
            { model
            | lists = updatedLists
            , newList = newInput
            , modal = Modal.hidden
            }


onNewListName : Config msg -> Model -> Input.Option msg
onNewListName config model =
    let
        updateName name =
            config.onChange
                { model
                | newList = Just name
                , error =
                    if listExists name model.lists then
                        Just ("List exists already: " ++ name)
                    else
                        Nothing
                }
    in
        Input.onInput updateName


listExists : String -> List ShoppingList -> Bool
listExists newListName lists =
    lists
        |> List.map .name
        >> List.any ((==) newListName)


showError : Model -> Html msg
showError model =
    case model.error of
        Just errorMsg ->
            Alert.simpleDanger [] [ text errorMsg ]

        Nothing ->
            div [] []
