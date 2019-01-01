module ListEdit exposing (Config, Model, init, view)

import Browser
import Html exposing (Attribute, Html, a, button, div, h1, input, label, text)
import Html.Attributes exposing (autofocus, checked, href, type_, value)
import Html.Events exposing (onClick, onInput)
import ShoppingList exposing (ShoppingList)
import Bootstrap.Grid as Grid
import Bootstrap.Utilities.Border as Border
import Bootstrap.CDN as CDN
import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Input as Input


type alias Model =
    { list : ShoppingList
    , newItem : Maybe String
    , error : Maybe String
    }


type alias Config msg =
    { onChange : Model -> msg
    , onExit : ShoppingList -> msg
    }


init : ShoppingList -> Model
init list =
    { list = list
    , newItem = Nothing
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


-- todo: gray checked items
items : Config msg -> Model -> Html msg
items config model =
    let
        toListItem item = div []
            [ item.name |>
                Checkbox.checkbox
                    [ Checkbox.checked item.checked
                    , onItemClick config model item.name
                    ]
            ]
    in
        div []
            ( model.list.items
                |> List.reverse
                >> List.map toListItem
            )


onItemClick : Config msg -> Model -> String -> Checkbox.Option msg
onItemClick config model item =
    let
        updatedLists =
            ShoppingList.checkItem model.list item
    in
        Checkbox.onCheck (\_ -> ( config.onChange { model | list = updatedLists } ))


addItem : Config msg -> Model -> Html msg
addItem config model =
    let
        disable =
            case model.error of
                Just _ ->
                    True

                _ ->
                    model.newItem == Nothing || model.newItem == Just ""

        danger =
            if model.error /= Nothing then
                [ Input.danger ]
            else
                []
    in
        Grid.row []
            [ Grid.col []
                [ Input.text (
                    [ onNewItemName config model
                    , Input.value (Maybe.withDefault "" model.newItem)
                    ] ++ danger )
                ]
            , Grid.col []
                [ Button.button
                    [ Button.disabled disable
                    , Button.primary
                    , onNewItemCreate config model
                    ]
                    [ text "add" ]
                ]
            ]


onNewItemName : Config msg -> Model -> Input.Option msg
onNewItemName config model =
    let
        updateName name =
            config.onChange { model
            | newItem = Just name
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
            case model.newItem of
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
                    , newItem = newItemInput
                }
            )


selectList : Config msg -> Model -> Html msg
selectList config model =
    div
        [ onClick (config.onExit model.list) ]
        [ a [ href "" ] [ text "Back to shopping list selection" ] ]


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
