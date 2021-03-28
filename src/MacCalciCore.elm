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


type Operation
    = Addition
    | Subtraction


type NumberType
    = Integer Int
    | Decimal Float


type alias Model =
    { firstNumber : NumberType
    , secondNumber : NumberType
    , displayedNumber : NumberType
    , operationType : Maybe Operation
    , result : NumberType
    }


init : Model
init =
    { firstNumber = Integer 0
    , secondNumber = Integer 0
    , displayedNumber = Integer 0
    , operationType = Nothing
    , result = Integer 0
    }


type Msg
    = AddNumbers
    | UpdateNumber NumberType
    | AllClearTextField
    | EqualsTo


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddNumbers ->
            { model
                | operationType = Just Addition
                , displayedNumber = model.firstNumber
            }

        UpdateNumber num ->
            let
                insertAtOnes x y =
                    case y of
                        Integer u ->
                            Integer ((x * 10) + u)

                        Decimal v ->
                            Decimal ((toFloat x * 10) + v)
            in
            case model.operationType of
                Nothing ->
                    { model
                        | displayedNumber = insertAtOnes model.displayedNumber num
                        , firstNumber = insertAtOnes model.displayedNumber num
                    }

                Just _ ->
                    { model
                        | displayedNumber = insertAtOnes model.secondNumber num
                        , secondNumber = insertAtOnes model.secondNumber num
                    }

        AllClearTextField ->
            init

        EqualsTo ->
            let
                calculateResult m =
                    case m.operationType of
                        Just Addition ->
                            m.firstNumber + m.secondNumber

                        Just Subtraction ->
                            1

                        Nothing ->
                            2
            in
            { model
                | result = calculateResult model
                , displayedNumber = calculateResult model
                , firstNumber = calculateResult model
            }


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ button [ onClick AllClearTextField ] [ text "AC" ]
            , input
                [ placeholder "Enter number"
                , value <|
                    case model.displayedNumber of
                        Integer x ->
                            String.fromInt x

                        Decimal y ->
                            String.fromFloat y
                ]
                []
            ]
        , button [ onClick <| UpdateNumber (Integer 7) ] [ text "7" ]
        , button [ onClick <| UpdateNumber (Integer 8) ] [ text "8" ]
        , button [ onClick <| UpdateNumber (Integer 9) ] [ text "9" ]
        , button [] [ text "/" ]
        , button [] [ text "%" ]
        , div []
            [ button [ onClick <| UpdateNumber (Integer 4) ] [ text "4" ]
            , button [ onClick <| UpdateNumber (Integer 5) ] [ text "5" ]
            , button [ onClick <| UpdateNumber (Integer 6) ] [ text "6" ]
            , button [] [ text "X" ]
            ]
        , div []
            [ button [ onClick <| UpdateNumber (Integer 1) ] [ text "1" ]
            , button [ onClick <| UpdateNumber (Integer 2) ] [ text "2" ]
            , button [ onClick <| UpdateNumber (Integer 3) ] [ text "3" ]
            , button [] [ text "-" ]
            ]
        , div []
            [ button [ onClick <| UpdateNumber (Integer 0) ] [ text "0" ]
            , button [] [ text "." ]
            , button [ onClick EqualsTo ] [ text "=" ]
            , button [ onClick AddNumbers ] [ text "+" ]
            ]
        ]
