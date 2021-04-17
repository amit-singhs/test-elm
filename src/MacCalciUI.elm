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


type Operation
    = Addition
    | Subtraction
    | Multiplication
    | Division


type Sign
    = Positive
    | Negative


type NumberType
    = Integer Int Sign
    | Decimal Int Int Sign


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
    { firstNumber = Integer 0 Positive
    , secondNumber = Integer 0 Positive
    , displayedNumber = Integer 0 Positive
    , operationType = Nothing
    , result = Integer 0 Positive
    , decimalButtonIsOn = False
    }


type Msg
    = AddNumbers
    | SubtractNumbers
    | MultiplyNumbers
    | DivideNumbers
    | DivideByHundred
    | InsertDigit Int
    | AllClearTextField
    | EqualsTo
    | DecimalButtonPressed
    | DoNothing String


removeDecimal floatNumber =
    if (floatNumber - toFloat (floor floatNumber)) /= 0 then
        removeDecimal (10 * floatNumber)

    else
        round floatNumber


renderNumberTypetoFloat : NumberType -> Float
renderNumberTypetoFloat numType =
    case numType of
        Integer i sign ->
            case sign of
                Negative ->
                    toFloat (negate i)

                Positive ->
                    toFloat i

        Decimal intNumber decimalPlaces sign ->
            case sign of
                Negative ->
                    toFloat (negate (intNumber / toFloat (10 ^ decimalPlaces)))

                Positive ->
                    toFloat intNumber / toFloat (10 ^ decimalPlaces)


renderFloatToNumberType : Float -> NumberType
renderFloatToNumberType floatNumber =
    if String.contains "." (String.fromFloat floatNumber) == False then
        if floatNumber < 0 then
            Integer (ceiling floatNumber) Negative

        else
            Integer (ceiling floatNumber) Positive

    else
        case List.head (List.reverse (String.split "." (String.fromFloat floatNumber))) of
            Just decimalPart ->
                if floatNumber < 0 then
                    Decimal (removeDecimal floatNumber) (String.length decimalPart) Negative

                else
                    Decimal (removeDecimal floatNumber) (String.length decimalPart) Positive

            Nothing ->
                Decimal 0 0 Positive


renderNumberTypetoString : NumberType -> String
renderNumberTypetoString numType =
    case numType of
        Integer i sign ->
            case sign of
                Negative ->
                    String.fromInt (negate i)

                Positive ->
                    String.fromInt i

        Decimal intNumber decimalPlace sign ->
            case sign of
                Negative ->
                    String.fromFloat (negate (toFloat intNumber / toFloat (10 ^ decimalPlace)))

                Positive ->
                    String.fromFloat (toFloat intNumber / toFloat (10 ^ decimalPlace))



-- Update


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
                        Integer v sign ->
                            case sign of
                                Negative ->
                                    Integer ((v * 10) + d) Negative

                                Positive ->
                                    Integer ((v * 10) + d) Positive

                        Decimal v decimalPlace sign ->
                            --Decimal (v + toFloat d / toFloat (10 ^ decimalPlace)) (decimalPlace + 1)
                            case sign of
                                Negative ->
                                    Decimal ((v * 10) + d) (decimalPlace + 1) Negative

                                Positive ->
                                    Decimal ((v * 10) + d) (decimalPlace + 1) Positive
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

        DivideByHundred ->
            { model
                | displayedNumber =
                    case model.displayedNumber of
                        Integer x sign ->
                            case sign of
                                Negative ->
                                    renderFloatToNumberType (toFloat (negate x) / 100)

                                Positive ->
                                    renderFloatToNumberType (toFloat x / 100)

                        Decimal x y sign ->
                            case sign of
                                Negative ->
                                    renderFloatToNumberType (renderNumberTypetoFloat (Decimal x y Negative) / 100)

                                Positive ->
                                    renderFloatToNumberType (renderNumberTypetoFloat (Decimal x y Positive) / 100)
            }

        EqualsTo ->
            let
                calculateResult m =
                    case m.operationType of
                        Just Addition ->
                            --m.firstNumber + m.secondNumber
                            case m.firstNumber of
                                Integer a aSign ->
                                    case aSign of
                                        Negative ->
                                            case m.secondNumber of
                                                Integer b bSign ->
                                                    case bSign of
                                                        Negative ->
                                                            Integer (negate a + negate b)

                                                        Positive ->
                                                            Integer (negate a + b)

                                        Positive ->
                                            case m.secondNumber of
                                                Integer b bSign ->
                                                    case bSign of
                                                        Negative ->
                                                            Integer (a + negate b)

                                                        Positive ->
                                                            Integer (a + b)

                                                Decimal c d cdSign ->
                                                    case cdSign of
                                                        Negative ->
                                                            renderFloatToNumberType (toFloat a + renderNumberTypetoFloat (Decimal c d Negative))

                                                        Positive ->
                                                            renderFloatToNumberType (toFloat a + renderNumberTypetoFloat (Decimal c d Positive))

                                Decimal e f efSign ->
                                    case m.secondNumber of
                                        Integer g gsign ->
                                            renderFloatToNumberType (renderNumberTypetoFloat (Decimal e f) + toFloat g)

                                        Decimal h i hiSign ->
                                            renderFloatToNumberType (renderNumberTypetoFloat (Decimal e f) + renderNumberTypetoFloat (Decimal h i))

                        --Integer 1
                        Just Subtraction ->
                            case m.firstNumber of
                                Integer a ->
                                    case m.secondNumber of
                                        Integer b ->
                                            Integer (a - b)

                                        Decimal c d ->
                                            renderFloatToNumberType (toFloat a - renderNumberTypetoFloat (Decimal c d))

                                Decimal e f ->
                                    case m.secondNumber of
                                        Integer g ->
                                            renderFloatToNumberType (renderNumberTypetoFloat (Decimal e f) - toFloat g)

                                        Decimal h i ->
                                            renderFloatToNumberType (renderNumberTypetoFloat (Decimal e f) - renderNumberTypetoFloat (Decimal h i))

                        Just Multiplication ->
                            case m.firstNumber of
                                Integer a ->
                                    case m.secondNumber of
                                        Integer b ->
                                            Integer (a * b)

                                        Decimal c d ->
                                            renderFloatToNumberType (toFloat a * renderNumberTypetoFloat (Decimal c d))

                                Decimal e f ->
                                    case m.secondNumber of
                                        Integer g ->
                                            renderFloatToNumberType (renderNumberTypetoFloat (Decimal e f) * toFloat g)

                                        Decimal h i ->
                                            renderFloatToNumberType (renderNumberTypetoFloat (Decimal e f) * renderNumberTypetoFloat (Decimal h i))

                        Just Division ->
                            --m.firstNumber + m.secondNumber
                            case m.firstNumber of
                                Integer a ->
                                    case m.secondNumber of
                                        Integer b ->
                                            Integer (a // b)

                                        Decimal c d ->
                                            renderFloatToNumberType (toFloat a / renderNumberTypetoFloat (Decimal c d))

                                Decimal e f ->
                                    case m.secondNumber of
                                        Integer g ->
                                            renderFloatToNumberType (renderNumberTypetoFloat (Decimal e f) / toFloat g)

                                        Decimal h i ->
                                            renderFloatToNumberType (renderNumberTypetoFloat (Decimal e f) / renderNumberTypetoFloat (Decimal h i))

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

        DoNothing str ->
            model

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


appendPeriodToInt : Int -> String
appendPeriodToInt integerToBeAppended =
    String.fromFloat (toFloat integerToBeAppended) ++ "."


view : Model -> Html Msg
view model =
    let
        createButton buttonLabel buttonlength buttonEvent tL tR bL bR backGroundRed backGroundGreen backGroundBlue onPressRed onPressBlue onPressGreen =
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
                , Background.color <| Element.rgb255 backGroundRed backGroundGreen backGroundBlue
                , Font.color <| Element.rgb255 228 228 228
                , Font.medium
                , Font.center
                , mouseDown
                    [ Background.color <| Element.rgb255 onPressRed onPressBlue onPressGreen
                    , Border.color <| Element.rgb255 84 83 81
                    ]
                ]
                { onPress = Just buttonEvent
                , label = text buttonLabel
                }
    in
    Element.layout [ padding 40 ] <|
        row []
            [ column [ width fill ]
                [ row []
                    [ Input.text
                        [ Border.roundEach
                            { topLeft = 12
                            , topRight = 12
                            , bottomLeft = 0
                            , bottomRight = 0
                            }
                        , Border.color <| Element.rgb255 84 83 81
                        , padding 40
                        , Background.color <| Element.rgb255 82 82 81
                        , Font.light
                        , Font.alignRight
                        , Font.color <| Element.rgb255 255 255 255
                        ]
                        { label = Input.labelHidden "Result output box"
                        , onChange = DoNothing
                        , placeholder = Nothing
                        , text = renderNumberTypetoString model.displayedNumber
                        }
                    ]
                , row []
                    [ column [] [ createButton "AC" 70 AllClearTextField 0 0 0 0 103 102 101 126 126 125 ]
                    , column [] [ createButton "+/-" 70 (DoNothing "") 0 0 0 0 103 102 101 126 126 125 ]
                    , column [] [ createButton "%" 70 DivideByHundred 0 0 0 0 103 102 101 126 126 125 ]
                    , column [] [ createButton "÷" 80 DivideNumbers 0 0 0 0 253 160 39 192 129 46 ]
                    ]
                , row []
                    [ column [] [ createButton "7" 70 (InsertDigit 7) 0 0 0 0 126 126 125 180 180 179 ]
                    , column [] [ createButton "8" 70 (InsertDigit 8) 0 0 0 0 126 126 125 180 180 179 ]
                    , column [] [ createButton "9" 70 (InsertDigit 9) 0 0 0 0 126 126 125 180 180 179 ]
                    , column [] [ createButton "X" 80 MultiplyNumbers 0 0 0 0 253 160 39 192 129 46 ]
                    ]
                , row []
                    [ column [] [ createButton "4" 70 (InsertDigit 4) 0 0 0 0 126 126 125 180 180 179 ]
                    , column [] [ createButton "5" 70 (InsertDigit 5) 0 0 0 0 126 126 125 180 180 179 ]
                    , column [] [ createButton "6" 70 (InsertDigit 6) 0 0 0 0 126 126 125 180 180 179 ]
                    , column [] [ createButton "—" 80 SubtractNumbers 0 0 0 0 253 160 39 192 129 46 ]
                    ]
                , row []
                    [ column [] [ createButton "1" 70 (InsertDigit 1) 0 0 0 0 126 126 125 180 180 179 ]
                    , column [] [ createButton "2" 70 (InsertDigit 2) 0 0 0 0 126 126 125 180 180 179 ]
                    , column [] [ createButton "3" 70 (InsertDigit 3) 0 0 0 0 126 126 125 180 180 179 ]
                    , column [] [ createButton "+" 80 AddNumbers 0 0 0 0 253 160 39 192 129 46 ]
                    ]
                , row []
                    [ column [] [ createButton "0" 140 (InsertDigit 0) 0 0 12 0 126 126 125 180 180 179 ]
                    , column [] [ createButton "." 70 DecimalButtonPressed 0 0 0 0 126 126 125 180 180 179 ]
                    , column [] [ createButton "=" 80 EqualsTo 0 0 0 12 253 160 39 192 129 46 ]
                    ]
                ]
            ]
