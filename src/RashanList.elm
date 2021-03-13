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
        []
    , newItem = ""
    , idCounter = 1
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
            case model.newItem of
                "" ->
                    model

                s ->
                    { model
                        | shoppingList = Item s True model.idCounter :: model.shoppingList
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
                    , row
                        [ padding 10
                        , Background.color <| Element.rgb255 212 244 246
                        ]
                        [ Input.button
                            [ padding 5
                            , Border.width 2
                            , Border.rounded 6
                            , Border.color <| Element.rgb255 110 150 150
                            , Background.color <| Element.rgb255 180 200 200
                            , Font.variant Font.smallCaps
                            , mouseDown
                                [ Background.color <| Element.rgb255 51 204 204
                                , Border.color <| Element.rgb255 51 204 204
                                , Font.color <| Element.rgb255 255 255 255
                                ]
                            , mouseOver
                                [ Background.color <| Element.rgb255 255 255 255
                                , Border.color <| Element.rgb255 180 180 190
                                ]
                            ]
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
                row
                    [ padding 6
                    , Events.onClick (ItemPurchased item)
                    ]
                    [ text (String.fromInt item.id ++ ".) " ++ item.label) ]
                    :: renderToList tail

            else
                row
                    [ padding 6
                    , Font.strike
                    , Events.onClick (ItemUnPurchased item)
                    ]
                    [ text (String.fromInt item.id ++ ".) " ++ item.label) ]
                    :: renderToList tail
