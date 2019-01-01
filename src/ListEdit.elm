module ListEdit exposing (Config, Model, init, view)

import Browser
import Html exposing (Attribute, Html, a, button, div, h1, input, text)
import Html.Attributes exposing (checked, href, type_, value)
import Html.Events exposing (onClick, onInput)
import ShoppingList exposing (ShoppingList)
import Bootstrap.Grid as Grid
import Bootstrap.Utilities.Border as Border
import Bootstrap.CDN as CDN

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
        [ Grid.container
            [ Border.all, Border.rounded ]
            [ Grid.row
                [ ]
                [ Grid.col
                    [ ]
                    [ CDN.stylesheet
                    , h1 [] [ text "Elm Shopping" ]
                    , div [] [ text ("Shopping list " ++ model.list.name) ]
                    , items config model
                    , addItem config model
                    , selectList config model
                    , showError model
                    ]
                ]
            ]
        ]
    }


-- todo: gray checked items
items : Config msg -> Model -> Html msg
items config model =
    let
        toListItem item = div []
            [ text item.name
            , input
                [ type_ "checkbox"
                , checked item.checked
                , onItemClick config model item.name
                ]
                []
            ]
    in
        div [] (List.map toListItem model.list.items)


onItemClick : Config msg -> Model -> String -> Attribute msg
onItemClick config model item =
    let
        updatedLists =
            ShoppingList.checkItem model.list item
    in
        onClick ( config.onChange { model | list = updatedLists } )


addItem : Config msg -> Model -> Html msg
addItem config model =
    div []
        [ input [ onNewItemName config model, value (Maybe.withDefault "" model.newItem) ] []
        , button [ onNewItemCreate config model ] [ text "create" ]
        ]


onNewItemName : Config msg -> Model -> Attribute msg
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
        onInput updateName


onNewItemCreate : Config msg -> Model -> Attribute msg
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
        onClick
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
            div [] [ text errorMsg ]

        Nothing ->
            div [] []
