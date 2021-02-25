module RashanList exposing (..)

import Browser
import Html exposing (Html, button, del, div, input, p, text)
import Html.Attributes exposing (id, placeholder, value)
import Html.Events exposing (onClick, onInput)
import List exposing (..)
import Main exposing (Model, init, update, view)



--Main


main =
    Browser.sandbox { init = init, view = view, update = update }



-- Model


type alias Item =
    { label : String
    , isPurchased : Bool
    , id : Int
    }


type alias Model =
    { shoppingList : List Item
    , newItem : String
    , idCounter : Int
    }


init : Model
init =
    { shoppingList = []
    , newItem = ""
    , idCounter = 0
    }


type Msg
    = Additem
    | UpdateNewItem String
    | ItemPurchased Item
    | ItemUnPurchased Item



-- Update


update : Msg -> Model -> Model
update msg model =
    case msg of
        Additem ->
            { model
                | shoppingList = Item model.newItem True model.idCounter :: model.shoppingList
                , newItem = ""
                , idCounter = model.idCounter + 1
            }

        UpdateNewItem inpTemp ->
            { model | newItem = inpTemp }

        ItemPurchased item ->
            { model
                | shoppingList =
                    { item | isPurchased = False }
                        :: filter (\it -> item.id /= it.id) model.shoppingList
            }

        ItemUnPurchased item ->
            { model
                | shoppingList =
                    { item | isPurchased = True }
                        :: filter (\it -> item.id /= it.id) model.shoppingList
            }



--View


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ input [ placeholder "Enter item", value model.newItem, onInput UpdateNewItem ] []
            , button [ onClick Additem ] [ text "Add item" ]
            , div [] <|
                renderToList <|
                    sortBy .id model.shoppingList
            ]
        ]


renderToList : List Item -> List (Html Msg)
renderToList xs =
    case xs of
        [] ->
            []

        item :: tail ->
            if item.isPurchased == True then
                p [ onClick (ItemPurchased item) ] [ text item.label ] :: renderToList tail

            else
                p [ onClick (ItemUnPurchased item) ] [ del [] [ text item.label ] ] :: renderToList tail
