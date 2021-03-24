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


type alias Model =
    { firstNumber : Int
    , secondNumber : Int
    , displayedNumber : Int
    , operationType : Maybe Operation
    , result : Int
    }


init : Model
init =
    { firstNumber = 0
    , secondNumber = 0
    , displayedNumber = 0
    , operationType = Nothing
    , result = 0
    }


type Msg
    = AddNumbers
    | UpdateNumber Int
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
                    (x * 10) + y
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
                    String.fromInt model.displayedNumber
                ]
                []
            ]
        , button [ onClick <| UpdateNumber 7 ] [ text "7" ]
        , button [ onClick <| UpdateNumber 8 ] [ text "8" ]
        , button [ onClick <| UpdateNumber 9 ] [ text "9" ]
        , button [] [ text "/" ]
        , button [] [ text "%" ]
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
            , button [ onClick EqualsTo ] [ text "=" ]
            , button [ onClick AddNumbers ] [ text "+" ]
            ]
        ]
