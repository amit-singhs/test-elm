module MacCalciCore exposing (..)

import Browser
import Element.Input exposing (username)
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (placeholder, style, value)
import Html.Events exposing (onClick, onInput)
import Main exposing (init, update, view)
import Round



--Main


main =
    Browser.sandbox { init = init, update = update, view = view }



--Model


type Operation
    = Addition
    | Subtraction
    | Multiplication
    | Division


type NumberType
    = Integer Int
    | Decimal Int Int


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
    | SubtractNumbers
    | MultiplyNumbers
    | DivideNumbers
    | InsertDigit Int
    | AllClearTextField
    | EqualsTo
    | DecimalButtonPressed


removeDecimal floatNumber =
    if (floatNumber - toFloat (floor floatNumber)) /= 0 then
        removeDecimal (10 * floatNumber)

    else
        round floatNumber


renderDecimaltoFloat : NumberType -> Float
renderDecimaltoFloat numType =
    case numType of
        Integer i ->
            toFloat i

        Decimal intNumber decimalPlaces ->
            toFloat intNumber / toFloat (10 ^ decimalPlaces)


renderFloatToDecimal : Float -> NumberType
renderFloatToDecimal floatNumber =
    if String.contains "." (String.fromFloat floatNumber) == False then
        Integer (ceiling floatNumber)

    else
        case List.head (List.reverse (String.split "." (String.fromFloat floatNumber))) of
            Just decimalPart ->
                Decimal (removeDecimal floatNumber) (String.length decimalPart)

            Nothing ->
                Decimal 0 0


renderDecimaltoString : NumberType -> String
renderDecimaltoString numType =
    case numType of
        Integer i ->
            String.fromInt i

        Decimal intNumber decimalPlace ->
            String.fromFloat (toFloat intNumber / toFloat (10 ^ decimalPlace))



--intNumber / (10 ^ decimalPlaces)


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddNumbers ->
            { model
                | operationType = Just Addition
                , displayedNumber = model.firstNumber
                , decimalButtonIsOn = False
            }

        SubtractNumbers ->
            { model
                | operationType = Just Subtraction
                , displayedNumber = model.firstNumber
                , decimalButtonIsOn = False
            }

        MultiplyNumbers ->
            { model
                | operationType = Just Multiplication
                , displayedNumber = model.firstNumber
                , decimalButtonIsOn = False
            }

        DivideNumbers ->
            { model
                | operationType = Just Division
                , displayedNumber = model.firstNumber
                , decimalButtonIsOn = False
            }

        InsertDigit digit ->
            let
                insertAtOnes : NumberType -> Int -> NumberType
                insertAtOnes x d =
                    case x of
                        Integer v ->
                            Integer ((v * 10) + d)

                        Decimal v decimalPlace ->
                            --Decimal (v + toFloat d / toFloat (10 ^ decimalPlace)) (decimalPlace + 1)
                            Decimal ((v * 10) + d) (decimalPlace + 1)
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
                            case m.firstNumber of
                                Integer a ->
                                    case m.secondNumber of
                                        Integer b ->
                                            Integer (a + b)

                                        Decimal c d ->
                                            renderFloatToDecimal (toFloat a + renderDecimaltoFloat (Decimal c d))

                                Decimal e f ->
                                    case m.secondNumber of
                                        Integer g ->
                                            renderFloatToDecimal (renderDecimaltoFloat (Decimal e f) + toFloat g)

                                        Decimal h i ->
                                            renderFloatToDecimal (renderDecimaltoFloat (Decimal e f) + renderDecimaltoFloat (Decimal h i))

                        --Integer 1
                        Just Subtraction ->
                            case m.firstNumber of
                                Integer a ->
                                    case m.secondNumber of
                                        Integer b ->
                                            Integer (a - b)

                                        Decimal c d ->
                                            renderFloatToDecimal (toFloat a - renderDecimaltoFloat (Decimal c d))

                                Decimal e f ->
                                    case m.secondNumber of
                                        Integer g ->
                                            renderFloatToDecimal (renderDecimaltoFloat (Decimal e f) - toFloat g)

                                        Decimal h i ->
                                            renderFloatToDecimal (renderDecimaltoFloat (Decimal e f) - renderDecimaltoFloat (Decimal h i))

                        Just Multiplication ->
                            case m.firstNumber of
                                Integer a ->
                                    case m.secondNumber of
                                        Integer b ->
                                            Integer (a * b)

                                        Decimal c d ->
                                            renderFloatToDecimal (toFloat a * renderDecimaltoFloat (Decimal c d))

                                Decimal e f ->
                                    case m.secondNumber of
                                        Integer g ->
                                            renderFloatToDecimal (renderDecimaltoFloat (Decimal e f) * toFloat g)

                                        Decimal h i ->
                                            renderFloatToDecimal (renderDecimaltoFloat (Decimal e f) * renderDecimaltoFloat (Decimal h i))

                        Just Division ->
                            --m.firstNumber + m.secondNumber
                            case m.firstNumber of
                                Integer a ->
                                    case m.secondNumber of
                                        Integer b ->
                                            Integer (a // b)

                                        Decimal c d ->
                                            renderFloatToDecimal (toFloat a / renderDecimaltoFloat (Decimal c d))

                                Decimal e f ->
                                    case m.secondNumber of
                                        Integer g ->
                                            renderFloatToDecimal (renderDecimaltoFloat (Decimal e f) / toFloat g)

                                        Decimal h i ->
                                            renderFloatToDecimal (renderDecimaltoFloat (Decimal e f) / renderDecimaltoFloat (Decimal h i))

                        --m.firstNumber - m.secondNumber
                        --Integer 3
                        Nothing ->
                            Integer 0
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
                            Decimal u 0

                        Decimal _ _ ->
                            num
            in
            case model.operationType of
                Nothing ->
                    { model
                        | firstNumber = convertToDecimal model.firstNumber
                        , displayedNumber = convertToDecimal model.displayedNumber
                        , result = convertToDecimal model.result
                        , decimalButtonIsOn = True
                    }

                _ ->
                    { model
                        | secondNumber = convertToDecimal model.secondNumber
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

                        Decimal y z ->
                            if z == 0 then
                                String.fromFloat (toFloat y) ++ "."

                            else
                                renderDecimaltoString (Decimal y z)
                ]
                []
            ]
        , button [ onClick <| InsertDigit 7 ] [ text "7" ]
        , button [ onClick <| InsertDigit 8 ] [ text "8" ]
        , button [ onClick <| InsertDigit 9 ] [ text "9" ]
        , button [ onClick DivideNumbers ] [ text "/" ]
        , button [] [ text "%" ]
        , div []
            [ button [ onClick <| InsertDigit 4 ] [ text "4" ]
            , button [ onClick <| InsertDigit 5 ] [ text "5" ]
            , button [ onClick <| InsertDigit 6 ] [ text "6" ]
            , button [ onClick MultiplyNumbers ] [ text "x" ]
            ]
        , div []
            [ button [ onClick <| InsertDigit 1 ] [ text "1" ]
            , button [ onClick <| InsertDigit 2 ] [ text "2" ]
            , button [ onClick <| InsertDigit 3 ] [ text "3" ]
            , button [ onClick SubtractNumbers ] [ text "-" ]
            ]
        , div []
            [ button [ onClick <| InsertDigit 0 ] [ text "0" ]
            , button [ onClick DecimalButtonPressed ] [ text "." ]
            , button [ onClick EqualsTo ] [ text "=" ]
            , button [ onClick AddNumbers ] [ text "+" ]
            ]
        ]
