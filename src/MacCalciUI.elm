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


type Operation
    = Addition
    | Subtraction
    | Multiplication
    | Division


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
    let
        createButton buttonLabel buttonlength tL tR bL bR r g b =
            Input.button
                [ height (px 60)
                , width (px buttonlength)
                , Border.width 1
                , Border.roundEach
                    { topLeft = tL
                    , topRight = tR
                    , bottomLeft = bL
                    , bottomRight = bR
                    }
                , Border.color <| Element.rgb255 84 83 81
                , Font.size 25
                , Font.family
                    [ Font.typeface "Helvetica"
                    ]
                , Background.color <| Element.rgb255 r g b
                , Font.color <| Element.rgb255 228 228 228
                , Font.medium
                , Font.center
                , mouseDown
                    [ Background.color <| Element.rgb255 180 180 179
                    , Border.color <| Element.rgb255 84 83 81
                    ]
                ]
                { onPress = Nothing
                , label = text buttonLabel
                }
    in
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
                        , Border.color <| Element.rgb255 84 83 81
                        , padding 40
                        , Background.color <| Element.rgb255 82 82 81
                        ]
                        { label = Input.labelHidden "Result output box"
                        , onChange = DoNothing
                        , placeholder = Just (Input.placeholder [ Font.size 60, Font.light, Font.alignRight, Font.color <| Element.rgb255 255 255 255 ] (text "0"))
                        , text = ""
                        }
                    ]
                , row []
                    [ column [] [ createButton "AC" 70 0 0 0 0 103 102 101 ]
                    , column [] [ createButton "+/-" 70 0 0 0 0 103 102 101 ]
                    , column [] [ createButton "%" 70 0 0 0 0 103 102 101 ]
                    , column [] [ createButton "÷" 80 0 0 0 0 242 163 60 ]
                    ]
                , row []
                    [ column [] [ createButton "7" 70 0 0 0 0 126 126 125 ]
                    , column [] [ createButton "8" 70 0 0 0 0 126 126 125 ]
                    , column [] [ createButton "9" 70 0 0 0 0 126 126 125 ]
                    , column [] [ createButton "X" 80 0 0 0 0 242 163 60 ]
                    ]
                , row []
                    [ column [] [ createButton "4" 70 0 0 0 0 126 126 125 ]
                    , column [] [ createButton "5" 70 0 0 0 0 126 126 125 ]
                    , column [] [ createButton "6" 70 0 0 0 0 126 126 125 ]
                    , column [] [ createButton "—" 80 0 0 0 0 242 163 60 ]
                    ]
                , row []
                    [ column [] [ createButton "1" 70 0 0 0 0 126 126 125 ]
                    , column [] [ createButton "2" 70 0 0 0 0 126 126 125 ]
                    , column [] [ createButton "3" 70 0 0 0 0 126 126 125 ]
                    , column [] [ createButton "+" 80 0 0 0 0 242 163 60 ]
                    ]
                , row []
                    [ column [] [ createButton "0" 140 0 0 12 0 126 126 125 ]
                    , column [] [ createButton "." 70 0 0 0 0 126 126 125 ]
                    , column [] [ createButton "=" 80 0 0 0 12 242 163 60 ]
                    ]
                ]
            ]
