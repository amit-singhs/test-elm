module MacCalciUI exposing (..)

import Browser
import Debug exposing (toString)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html, button, del, div, input, p)
import List exposing (..)
import Main exposing (Model, init, update, view)
import String



--Main


main =
    Browser.sandbox { init = init, view = view, update = update }


type alias Model =
    { theNumber : Int
    }


type Msg
    = UpdateTheNumber
    | DoNothing String


init : Model
init =
    { theNumber = 56
    }



-- Update


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateTheNumber ->
            { model | theNumber = model.theNumber + 1 }

        DoNothing emptyString ->
            model


view : Model -> Html Msg
view model =
    Element.layout [ padding 40 ] <|
        row []
            [ column [ width fill ]
                [ row []
                    [ Input.text
                        [ width fill
                        , Border.roundEach
                            { topLeft = 12
                            , topRight = 12
                            , bottomLeft = 0
                            , bottomRight = 0
                            }
                        , padding 40
                        , Background.color <| Element.rgb255 87 86 85
                        ]
                        { label = Input.labelHidden "Result output box"
                        , onChange = DoNothing
                        , placeholder = Just (Input.placeholder [] (text ""))
                        , text = ""
                        }
                    ]
                , row []
                    [ column []
                        [ Input.button
                            [ padding 19
                            , Border.width 1
                            , Border.color <| Element.rgb255 84 83 81
                            , Background.color <| Element.rgb255 103 102 101
                            , Font.color <| Element.rgb255 228 228 228
                            , Font.variant Font.smallCaps
                            , mouseDown
                                [ Background.color <| Element.rgb255 51 204 204
                                , Border.color <| Element.rgb255 51 204 204
                                , Font.color <| Element.rgb255 232 232 232
                                ]
                            , mouseOver
                                [ Background.color <| Element.rgb255 255 255 255
                                , Border.color <| Element.rgb255 180 180 190
                                ]
                            ]
                            { onPress = Nothing
                            , label = text "AC"
                            }
                        ]
                    ]
                ]
            ]
