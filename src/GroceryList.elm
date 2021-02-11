module GroceryList exposing (..)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)
import Main exposing (init, update, view)



{-
   Design a calculator
   Input A
   Input B
   Button +
   Button -
   Button *

   Result = A + B
-}


main =
    Browser.sandbox { init = init, update = update, view = view }



-- Model


type alias Model =
    { num1 : Int
    , num2 : Int
    , result : Int
    }


init : Model
init =
    { num1 = 0
    , num2 = 0
    , result = 0
    }


type Msg
    = AddNumbers
    | SubtractNumbers
    | MultiplyNumbers
    | UpdateA String
    | UpdateB String



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddNumbers ->
            { model | result = model.num1 + model.num2 }

        SubtractNumbers ->
            { model | result = model.num1 - model.num2 }

        MultiplyNumbers ->
            { model | result = model.num1 * model.num2 }

        UpdateA inp1 ->
            { model | num1 = Maybe.withDefault 0 (String.toInt inp1) }

        UpdateB inp2 ->
            { model | num2 = Maybe.withDefault 0 (String.toInt inp2) }



-- model.num2
-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick AddNumbers ] [ text "Add" ]
        , button [ onClick SubtractNumbers ] [ text "Subtract" ]
        , button [ onClick MultiplyNumbers ] [ text "Multiply" ]
        , div []
            [ input [ placeholder "Enter first number", onInput UpdateA ] []
            , input [ placeholder "Enter Second number", onInput UpdateB ] []
            , div [] [ text (String.fromInt model.result) ]
            ]
        ]
