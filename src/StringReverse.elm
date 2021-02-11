module StringReverse exposing (..)

import Browser
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (placeholder, style, value)
import Html.Events exposing (onInput)
import Main exposing (Model, Msg, init, update, view)
import String exposing (reverse)



--Main


main =
    Browser.sandbox { init = init, update = update, view = view }



--Model


type alias Model =
    { inputString : String
    }


init : Model
init =
    { inputString = "Hi"
    }


type Msg
    = InputUpdate String


update : Msg -> Model -> Model
update msg model =
    case msg of
        InputUpdate newString ->
            { model | inputString = newString }



--View


view : Model -> Html Msg
view model =
    div [ style "color" "blue" ]
        [ text model.inputString
        , div []
            [ input
                [ placeholder "Enter text"
                , value model.inputString
                , onInput InputUpdate
                ]
                []
            , text (String.reverse model.inputString)
            ]
        ]
