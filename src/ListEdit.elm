module ListEdit exposing (Config, Model, init, view)

import Browser
import Html exposing (Html, a, div, h1, text)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import ShoppingList exposing (ShoppingList)
import Bootstrap.Grid as Grid
import Bootstrap.Utilities.Border as Border
import Bootstrap.CDN as CDN
import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Input as Input
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Form.InputGroup as InputGroup


type alias Model =
    { list : ShoppingList
    , newItemName : Maybe String
    , error : Maybe String
    }


type alias Config msg =
    { onChange : Model -> msg
    , onExit : ShoppingList -> msg
    }


init : ShoppingList -> Model
init list =
    { list = list
    , newItemName = Nothing
    , error = Nothing
    }


view : Config msg -> Model -> Browser.Document msg
view config model =
    { title = "Elm Shopping"
    , body =
        [ CDN.stylesheet
        , Grid.container
            [ Border.all, Border.rounded ]
            [ h1 [] [ text "Elm Shopping" ]
            , div [] [ text ("Shopping list " ++ model.list.name) ]
            , items config model
            , addItem config model
            , selectList config model
            , showError model
            ]
        ]
    }


items : Config msg -> Model -> Html msg
items config model =
    let
        attrs item =
            if item.checked then
                [ ListGroup.disabled ]
            else
                []

        toListItem item =
            ListGroup.li
                (attrs item)
                [ Checkbox.checkbox
                    [ Checkbox.checked item.checked
                    , onItemClick item.name config model
                    ]
                    item.name
                ]
    in
        ListGroup.ul
            ( model.list.items
                |> List.reverse
                |> List.map toListItem
            )


onItemClick : String -> Config msg -> Model -> Checkbox.Option msg
onItemClick item config model =
    let
        updatedLists =
            ShoppingList.checkItem model.list item
    in
        Checkbox.onCheck <| \_ -> config.onChange
            { model | list = updatedLists }


addItem : Config msg -> Model -> Html msg
addItem config model =
    let
        disable
            =  model.error /= Nothing
            || model.newItemName == Nothing
            || model.newItemName == Just ""

        danger =
            if model.error /= Nothing then
                [ Input.danger ]
            else
                []

        textInputField =
            InputGroup.text
                (
                [ Input.placeholder "New item"
                , onNewItemName config model
                , Input.value (Maybe.withDefault "" model.newItemName)
                ] ++ danger
                )

        addButton =
            InputGroup.button
                [ Button.disabled disable
                , Button.primary
                , onNewItemCreate config model
                ]
                [ text "Add"]
    in
        Grid.row []
            [ Grid.col [ ]
                [ InputGroup.config textInputField
                    |> InputGroup.successors [ addButton ]
                    |> InputGroup.view
                ]
            ]


onNewItemName : Config msg -> Model -> Input.Option msg
onNewItemName config model =
    let
        updateName name =
            config.onChange
            { model
            | newItemName = Just name
            , error =
                if itemExists name model.list then
                    Just ("Item exists: " ++ name)
                else
                    Nothing
            }
    in
        Input.onInput updateName


onNewItemCreate : Config msg -> Model -> Button.Option msg
onNewItemCreate config model =
    let
        (updatedLists, newItemInput) =
            case model.newItemName of
                Just name ->
                    if itemExists name model.list then
                        (model.list, Just name)
                    else
                        (ShoppingList.addItem model.list name, Nothing)

                Nothing ->
                    (model.list, Nothing)
    in
        Button.onClick
            ( config.onChange
                { model
                | list = updatedLists
                , newItemName = newItemInput
                }
            )


selectList : Config msg -> Model -> Html msg
selectList config model =
    div
        [ onClick << config.onExit <| model.list ]
        [ a [ href "#" ] [ text "Back to shopping list selection" ] ]


itemExists : String -> ShoppingList -> Bool
itemExists newItem shoppingList =
    shoppingList.items
        |> List.map .name
        >> List.any ((==) newItem)


showError : Model -> Html msg
showError model =
    case model.error of
        Just errorMsg ->
            Alert.simpleDanger [] [ text errorMsg ]

        Nothing ->
            div [] []
