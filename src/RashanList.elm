module RashanList exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Html exposing (Html)
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


bgcolor1 =
    Background.color <| rgb255 255 55 55


bgcolor2 =
    Background.color <| rgb255 55 55 255


view : Model -> Html msg
view model =
    layout
        [ width fill
        , height fill
        ]
    <|
        row
            [ Background.color <| rgb255 23 232 65
            , width fill
            ]
            [ column
                [ width fill
                , height fill
                ]
                [ row [] [ text "Rashan List" ]
                , row [ height fill ] [ text "" ]
                ]
            , column [ width fill ]
                [ row [] [ text "Item 1" ]
                , row [] [ text "Item 2" ]
                , row [] [ text "Item 3" ]
                , row [] [ text "Item 4" ]
                ]
            ]



{-
   view2 : Model -> Html Msg
   view2 model =
       div []
           [ div []
               [ input [ placeholder "Enter item", value model.newItem, onInput UpdateNewItem ] []
               , button [ onClick Additem ] [ Html.text "Add item" ]
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
                   p [ onClick (ItemPurchased item) ] [ Html.text item.label ] :: renderToList tail

               else
                   p [ onClick (ItemUnPurchased item) ] [ del [] [ Html.text item.label ] ] :: renderToList tail

-}
