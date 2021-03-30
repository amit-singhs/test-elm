module MacCalciCore exposing (..)

import Browser
import Element.Input exposing (username)
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
    | Decimal Float Int


type alias Model =
    { firstNumber : NumberType
    , secondNumber : NumberType
    , displayedNumber : NumberType
    , operationType : Maybe Operation
    , result : NumberType
    , decimalButtonIsOn : Bool
    }


init : Model
init =
    { firstNumber = Integer 0
    , secondNumber = Integer 0
    , displayedNumber = Integer 0
    , operationType = Nothing
    , result = Integer 0
    , decimalButtonIsOn = False
    }


type Msg
    = AddNumbers
    | InsertDigit Int
    | AllClearTextField
    | EqualsTo
    | DecimalButtonPressed


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddNumbers ->
            { model
                | operationType = Just Addition
                , displayedNumber = model.firstNumber
            }

        InsertDigit digit ->
            let
                insertAtOnes : NumberType -> Int -> NumberType
                insertAtOnes x d =
                    case x of
                        Integer v ->
                            Integer ((v * 10) + d)

                        Decimal v decimalPlace ->
                            Decimal (v + toFloat d / toFloat (10 ^ decimalPlace)) (decimalPlace + 1)
            in
            case model.operationType of
                Nothing ->
                    { model
                        | displayedNumber = insertAtOnes model.displayedNumber digit
                        , firstNumber = insertAtOnes model.displayedNumber digit
                    }

                Just _ ->
                    { model
                        | displayedNumber = insertAtOnes model.secondNumber digit
                        , secondNumber = insertAtOnes model.secondNumber digit
                    }

        AllClearTextField ->
            init

        EqualsTo ->
            let
                calculateResult m =
                    case m.operationType of
                        Just Addition ->
                            --m.firstNumber + m.secondNumber
                            Integer 1

                        Just Subtraction ->
                            --m.firstNumber - m.secondNumber
                            Integer 3

                        Nothing ->
                            Integer 2
            in
            { model
                | result = calculateResult model
                , displayedNumber = calculateResult model
                , firstNumber = calculateResult model
            }

        DecimalButtonPressed ->
            let
                convertToDecimal num =
                    case num of
                        Integer u ->
                            Decimal (toFloat u) 1

                        Decimal _ _ ->
                            num
            in
            { model
                | firstNumber = convertToDecimal model.firstNumber
                , secondNumber = convertToDecimal model.secondNumber
                , displayedNumber = convertToDecimal model.displayedNumber
                , result = convertToDecimal model.result
                , decimalButtonIsOn = True
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

                        Decimal y _ ->
                            String.fromFloat y
                ]
                []
            ]
        , button [ onClick <| InsertDigit 7 ] [ text "7" ]
        , button [ onClick <| InsertDigit 8 ] [ text "8" ]
        , button [ onClick <| InsertDigit 9 ] [ text "9" ]
        , button [] [ text "/" ]
        , button [] [ text "%" ]
        , div []
            [ button [ onClick <| InsertDigit 4 ] [ text "4" ]
            , button [ onClick <| InsertDigit 5 ] [ text "5" ]
            , button [ onClick <| InsertDigit 6 ] [ text "6" ]
            , button [] [ text "x" ]
            ]
        , div []
            [ button [ onClick <| InsertDigit 1 ] [ text "1" ]
            , button [ onClick <| InsertDigit 2 ] [ text "2" ]
            , button [ onClick <| InsertDigit 3 ] [ text "3" ]
            , button [] [ text "-" ]
            ]
        , div []
            [ button [ onClick <| InsertDigit 0 ] [ text "0" ]
            , button [ onClick DecimalButtonPressed ] [ text "." ]
            , button [ onClick EqualsTo ] [ text "=" ]
            , button [ onClick AddNumbers ] [ text "+" ]
            ]
        ]
