module ListSelect exposing (Config, Model, init, view)

import Browser
import Html exposing (Attribute, Html, a, button, div, h1, input, text)
import Html.Attributes exposing (href, value)
import Html.Events exposing (onClick, onInput)
import ShoppingList exposing (ShoppingList)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Utilities.Border as Border
import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.Form.Input as Input
import Bootstrap.Grid.Col as Col

type alias Model =
    { lists : List ShoppingList
    , newList : Maybe String
    , error : Maybe String
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
            , newList config model
            , showError model
            ]
        ]
    }

test1 : Html msg
test1 =
    Grid.container []
        [ CDN.stylesheet -- creates an inline style node with the Bootstrap CSS
        , Grid.row []
            [ Grid.col []
                [ text "Some content for my view here..."]
            ]

        ]

existingLists : Config msg -> Model -> Html msg
existingLists config model =
    let
        toListItem list =
            div [ onClick (config.onSelect model.lists list) ] [ a [ href "" ] [ text list.name ]]
    in
        div []
            ( model.lists
                |> List.reverse
                >> List.map toListItem
            )


newList : Config msg -> Model -> Html msg
newList config model =
    let
        disable
            =  model.error /= Nothing
            || model.newList == Nothing
            || model.newList == Just ""

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
            , Grid.col [ ]
                [ Button.button
                    [ Button.disabled disable
                    , Button.primary
                    , onNewListCreate config model
                    ]
                    [ text "create" ]
                ]
            ]


onNewListName : Config msg -> Model -> Input.Option msg
onNewListName config model =
    let
        updateName name =
            config.onChange { model
                | newList = Just name
                , error =
                    if listExists name model.lists then
                        Just ("List exists: " ++ name)
                    else
                        Nothing
                }
    in
        Input.onInput updateName


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
        Button.onClick
            ( config.onChange { model
                | lists = updatedLists
                , newList = newInput
                }
            )


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
