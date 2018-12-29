module ListSelect exposing (Model, Msg, init, view)

import Browser
import Html exposing (Html, a, button, div, h1, input, text)
import Html.Attributes exposing (href)


type alias Model =
    { lists : List ShoppingList
    }


type alias ShoppingList =
    { name : String
    }


type Msg
    = Foo Int
    | Bar String


init : Model
init = Model [ ShoppingList "list one", ShoppingList "list two"]


view : Model -> Browser.Document Msg
view model =
    { title = "Elm Shopping"
    , body =
        [ h1 [] [ text "Elm Shopping" ]
        , div [] [ text "Choose or create shopping list" ]
        , existingLists model
        , newList model
        ]
    }

existingLists : Model -> Html Msg
existingLists model =
    div [] (List.map toListItem model.lists)

toListItem : ShoppingList -> Html Msg
toListItem shoppingList =
    div [] [ a [ href "" ] [ text shoppingList.name ]]


newList : Model -> Html Msg
newList model =
    div []
        [ input [] []
        , button [] [ text "create" ]
        ]