module RashanList exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html, button, del, div, input, p)
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
    { shoppingList =
        [ { label = "Apples", isPurchased = False, id = 1 }
        , { label = "Eggs", isPurchased = False, id = 2 }
        , { label = "Bananas", isPurchased = False, id = 3 }
        ]
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



--view : Model -> Html msg


view model =
    layout
        [ Background.color <| rgb255 212 244 246
        , width fill
        , height fill
        , padding 10
        ]
    <|
        row
            [ width fill
            ]
            [ column
                [ width fill
                , height fill
                , padding 5
                ]
                [ row
                    [ Font.color (Element.rgb255 56 125 138)
                    , Font.size 36
                    , Font.heavy
                    , Font.family
                        [ Font.external
                            { name = "Libre Franklin"
                            , url = "https://fonts.googleapis.com/css2?family=Libre+Franklin:wght@800&display=swap"
                            }
                        , Font.sansSerif
                        ]
                    ]
                    [ text "Rashan List" ]
                , row
                    []
                    [ Input.text []
                        { label = Input.labelHidden "Add new item"
                        , onChange = UpdateNewItem
                        , placeholder = Just (Input.placeholder [] (text "Add new item"))
                        , text = model.newItem
                        }
                    , row []
                        [ Input.button []
                            { onPress = Just Additem
                            , label = text "Add item"
                            }
                        ]
                    ]
                ]
            , column
                [ width fill
                , padding 5
                ]
                (renderToList (sortBy .id model.shoppingList))
            ]


renderToList xs =
    case xs of
        [] ->
            [ row [] [ text "" ] ]

        item :: tail ->
            if item.isPurchased == True then
                row [ Events.onClick (ItemPurchased item) ] [ text item.label ] :: renderToList tail

            else
                row
                    [ Font.strike
                    , Events.onClick (ItemUnPurchased item)
                    ]
                    [ text item.label ]
                    :: renderToList tail
