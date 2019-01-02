module ShoppingList exposing
    ( ShoppingList
    , newList
    , addItem
    , checkItem
    , itemCount
    , checkedItemCount
    )

type alias ShoppingList =
    { name : String
    , items : List Item
    }


type alias Item =
    { name : String
    , checked : Bool
    }


newList : String -> ShoppingList
newList name = ShoppingList name []


addItem : ShoppingList -> String -> ShoppingList
addItem list item =
    let
        newItem =
            { name = item
            , checked = False
            }
    in
        { list | items = newItem :: list.items}


checkItem : ShoppingList -> String -> ShoppingList
checkItem list itemName =
    let
        check name item =
            if (item.name == name) then
                { item | checked = True }
            else
                item

        newItems =
            List.map (check itemName) list.items
    in
        { list | items = newItems }


itemCount : ShoppingList -> Int
itemCount shoppingList =
    List.length shoppingList.items


checkedItemCount : ShoppingList -> Int
checkedItemCount shoppingList =
    let
        isChecked item = item.checked
    in
        List.filter isChecked shoppingList.items
            |> List.length
