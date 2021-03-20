module MacCalciCore exposing (..)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (placeholder, style, value)
import Html.Events exposing (onClick, onInput)
import Main exposing (init, update, view)



--Main


main =
    Browser.sandbox { init = init, update = update, view = view }



--Model


type alias Model =
    { firstNumber : Int
    , secondNumber : Int
    , displayedNumber : Int
    , mathematicalOperationIsOn : Bool
    , result : Int
    }


init : Model
init =
    { firstNumber = 0
    , secondNumber = 0
    , displayedNumber = 0
    , mathematicalOperationIsOn = False
    , result = 0
    }


type Msg
    = AddNumbers
    | UpdateNumber Int
    | AllClearTextField


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddNumbers ->
            { model
                | displayedNumber = 0
                , mathematicalOperationIsOn = True
                , result = model.firstNumber + model.secondNumber
            }

        UpdateNumber num ->
            case model.mathematicalOperationIsOn of
                False ->
                    { model
                        | displayedNumber = (model.displayedNumber * 10) + num
                        , firstNumber = (model.displayedNumber * 10) + num
                    }

                True ->
                    { model
                        | displayedNumber = (model.displayedNumber * 10) + num
                        , secondNumber = (model.displayedNumber * 10) + num
                    }

        AllClearTextField ->
            { model | firstNumber = 0 }


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ button [ onClick AllClearTextField ] [ text "AC" ]
            , input [ placeholder "Enter number", value <| String.fromInt model.displayedNumber ] []
            ]
        , button [ onClick <| UpdateNumber 7 ] [ text "7" ]
        , button [ onClick <| UpdateNumber 8 ] [ text "8" ]
        , button [ onClick <| UpdateNumber 9 ] [ text "9" ]
        , button [] [ text "/" ]
        , div []
            [ button [ onClick <| UpdateNumber 4 ] [ text "4" ]
            , button [ onClick <| UpdateNumber 5 ] [ text "5" ]
            , button [ onClick <| UpdateNumber 6 ] [ text "6" ]
            , button [] [ text "X" ]
            ]
        , div []
            [ button [ onClick <| UpdateNumber 1 ] [ text "1" ]
            , button [ onClick <| UpdateNumber 2 ] [ text "2" ]
            , button [ onClick <| UpdateNumber 3 ] [ text "3" ]
            , button [] [ text "-" ]
            ]
        , div []
            [ button [ onClick <| UpdateNumber 0 ] [ text "0" ]
            , button [] [ text "." ]
            , button [] [ text "=" ]
            , button [ onClick AddNumbers ] [ text "+" ]
            ]
        ]
