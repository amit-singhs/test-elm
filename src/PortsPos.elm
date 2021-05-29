module PortsPos exposing (..)

--import Element.Text as text exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input exposing (username)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Main exposing (Msg(..))



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { displayedNumber : NumberType
    , inputNumber : NumberType
    , numbersList : List NumberType
    , total : NumberType
    }


type NumberType
    = Integer Int
    | Decimal Int Int



-- UPDATE


type Msg
    = SayHello
    | DoNothing String
    | InsertDigit Int
    | DecimalButtonPressed
    | AddButtonPressed


removeDecimal floatNumber =
    if (floatNumber - toFloat (floor floatNumber)) /= 0 then
        removeDecimal (10 * floatNumber)

    else
        round floatNumber


renderNumberTypetoFloat : NumberType -> Float
renderNumberTypetoFloat numType =
    case numType of
        Integer i ->
            toFloat i

        Decimal intNumber decimalPlaces ->
            toFloat intNumber / toFloat (10 ^ decimalPlaces)


renderFloatToNumberType : Float -> NumberType
renderFloatToNumberType floatNumber =
    if String.contains "." (String.fromFloat floatNumber) == False then
        Integer (ceiling floatNumber)

    else
        case List.head (List.reverse (String.split "." (String.fromFloat floatNumber))) of
            Just decimalPart ->
                Decimal (removeDecimal floatNumber) (String.length decimalPart)

            Nothing ->
                Decimal 0 0


renderNumberTypetoString : NumberType -> String
renderNumberTypetoString numType =
    case numType of
        Integer i ->
            String.fromInt i

        Decimal intNumber decimalPlace ->
            String.fromFloat (toFloat intNumber / toFloat (10 ^ decimalPlace))


addTwoNumberTypes : NumberType -> NumberType -> NumberType
addTwoNumberTypes numType1 numType2 =
    case numType1 of
        Integer i ->
            case numType2 of
                Integer j ->
                    Integer (i + j)

                Decimal _ _ ->
                    renderFloatToNumberType (toFloat i + renderNumberTypetoFloat numType2)

        Decimal _ _ ->
            case numType2 of
                Integer i ->
                    renderFloatToNumberType (toFloat i + renderNumberTypetoFloat numType1)

                Decimal _ _ ->
                    renderFloatToNumberType (renderNumberTypetoFloat numType1 + renderNumberTypetoFloat numType2)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { displayedNumber = Integer 0
      , inputNumber = Integer 0
      , numbersList = [ Integer 0 ]
      , total = Integer 0
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SayHello ->
            ( { model | displayedNumber = model.inputNumber }
            , Cmd.none
            )

        DoNothing inputString ->
            ( { model | displayedNumber = model.inputNumber }
            , Cmd.none
            )

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
            ( { model
                | displayedNumber = insertAtOnes model.displayedNumber digit
                , inputNumber = insertAtOnes model.displayedNumber digit
              }
            , Cmd.none
            )

        DecimalButtonPressed ->
            let
                convertToDecimal num =
                    case num of
                        Integer u ->
                            Decimal u 0

                        Decimal _ _ ->
                            num
            in
            ( { model
                | displayedNumber = convertToDecimal model.displayedNumber
                , inputNumber = convertToDecimal model.inputNumber
              }
            , Cmd.none
            )

        AddButtonPressed ->
            ( { model | total = addTwoNumberTypes model.total model.inputNumber }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    let
        createButton buttonLabel buttonlength buttonEvent tL tR bL bR r g b =
            Input.button
                [ Element.height (px 60)
                , Element.width (px buttonlength)
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
                { onPress = Just buttonEvent
                , label = Element.text buttonLabel
                }
    in
    Element.layout [] <|
        row [ padding 40 ]
            [ column [ Element.width fill ]
                [ row []
                    [ Input.text
                        [ Border.roundEach
                            { topLeft = 12
                            , topRight = 12
                            , bottomLeft = 0
                            , bottomRight = 0
                            }
                        , Element.width (px 495)
                        , Border.color <| Element.rgb255 84 83 81
                        , padding 40
                        , Background.color <| Element.rgb255 174 245 189
                        , Font.light
                        , Font.alignRight
                        , Font.color <| Element.rgb255 0 0 0
                        ]
                        { label = Input.labelHidden "input text box"
                        , onChange = DoNothing
                        , placeholder = Nothing
                        , text = renderNumberTypetoString model.displayedNumber
                        }
                    ]
                , row []
                    [ column [] [ createButton "1" 165 (InsertDigit 1) 0 0 0 0 106 166 119 ]
                    , column [] [ createButton "2" 165 (InsertDigit 2) 0 0 0 0 106 166 119 ]
                    , column [] [ createButton "3" 165 (InsertDigit 3) 0 0 0 0 106 166 119 ]
                    ]
                , row []
                    [ column [] [ createButton "4" 165 (InsertDigit 4) 0 0 0 0 106 166 119 ]
                    , column [] [ createButton "5" 165 (InsertDigit 5) 0 0 0 0 106 166 119 ]
                    , column [] [ createButton "6" 165 (InsertDigit 6) 0 0 0 0 106 166 119 ]
                    ]
                , row []
                    [ column [] [ createButton "7" 165 (InsertDigit 7) 0 0 0 0 106 166 119 ]
                    , column [] [ createButton "8" 165 (InsertDigit 8) 0 0 0 0 106 166 119 ]
                    , column [] [ createButton "9" 165 (InsertDigit 9) 0 0 0 0 106 166 119 ]
                    ]
                , row []
                    [ column [] [ createButton "." 165 DecimalButtonPressed 0 0 0 0 106 166 119 ]
                    , column [] [ createButton "0" 165 (InsertDigit 0) 0 0 0 0 106 166 119 ]
                    , column [] [ createButton "Add" 165 (DoNothing "") 0 0 0 0 106 166 119 ]
                    ]
                , row []
                    [ column [] [ createButton ("Pay: $ " ++ renderNumberTypetoString model.total) 495 (DoNothing "") 0 0 0 0 106 166 119 ]
                    ]
                ]
            , column [ padding 10 ] []
            , column []
                [ row []
                    [ Input.text
                        [ Border.roundEach
                            { topLeft = 12
                            , topRight = 12
                            , bottomLeft = 12
                            , bottomRight = 12
                            }
                        , Element.width (px 250)
                        , Element.height (px 650)
                        , Border.color <| Element.rgb255 84 83 81
                        , padding 40
                        , Background.color <| Element.rgb255 174 245 189
                        , Font.light
                        , Font.alignRight
                        , Font.color <| Element.rgb255 0 0 0
                        ]
                        { label = Input.labelHidden "text box"
                        , onChange = DoNothing
                        , placeholder = Nothing
                        , text = "0"
                        }
                    ]
                , row [ padding 20 ] [ Element.text ("Total : $ " ++ renderNumberTypetoString model.total) ]
                ]
            ]
