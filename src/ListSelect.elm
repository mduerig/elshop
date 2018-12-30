module ListSelect exposing (ShoppingList, Config, Model, init, view)

import Browser
import Html exposing (Attribute, Html, a, button, div, h1, input, text)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick, onInput)


type alias Model =
    { lists : List ShoppingList
    , newList : Maybe String
    }


type alias ShoppingList =
    { name : String
    }


type alias Config msg =
    { onChange : Model -> msg
    , onSelect : ShoppingList -> msg
    }

init : Model
init =
    { lists = [ ShoppingList "list one", ShoppingList "list two" ]
    , newList = Nothing
    }


view : Config msg -> Model -> Browser.Document msg
view config model =
    { title = "Elm Shopping"
    , body =
        [ h1 [] [ text "Elm Shopping" ]
        , div [] [ text "Choose or create shopping list" ]
        , existingLists config model
        , newList config model
        ]
    }


existingLists : Config msg -> Model -> Html msg
existingLists config model =
    let
        toListItem list =
            div [ onClick (config.onSelect list) ] [ a [ href "" ] [ text list.name ]]
    in
        div [] (List.map toListItem model.lists)


newList : Config msg -> Model -> Html msg
newList config model =
    div []
        [ input [ onNewListName config model ] []
        , button [ onNewListCreate config model ] [ text "create" ]
        ]


onNewListName : Config msg -> Model -> Attribute msg
onNewListName config model =
    let
        updateName name =
            config.onChange { model | newList = Just name }
    in
        onInput updateName


onNewListCreate : Config msg -> Model -> Attribute msg
onNewListCreate config model =
    let
        updatedLists =
            case model.newList of
                Just name ->
                    ShoppingList name :: model.lists

                Nothing ->
                    model.lists
    in
        onClick ( config.onChange { model | lists = updatedLists } )

