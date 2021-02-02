module Main exposing (..)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (attribute, name, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { name : String
    , password : String
    , passwordAgain : String
    , validationMessage : String
    }


init : Model
init =
    { name = ""
    , password = ""
    , passwordAgain = ""
    , validationMessage = ""
    }



-- UPDATE


type Msg
    = NameUpdate String
    | PasswordUpdate String
    | PasswordAgainUpdate String


update : Msg -> Model -> Model
update msg model =
    case msg of
        NameUpdate newName ->
            { model | name = newName }

        PasswordUpdate newPassword ->
            { model | password = newPassword }

        PasswordAgainUpdate newPassAgainUpdate ->
            { model
                | validationMessage =
                    if model.password == newPassAgainUpdate then
                        "Passwords are a match."

                    else
                        "Passwords do not match, Sorry."
                , passwordAgain = newPassAgainUpdate
            }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ input [ placeholder "Name", value model.name, onInput NameUpdate ] []
            , input [ placeholder "Password", type_ "password", onInput PasswordUpdate ] []
            , input [ placeholder "Password again", type_ "password", onInput PasswordAgainUpdate ] []
            , button [] [ text "Submit" ]
            ]
        , div [] [ text model.validationMessage ]
        ]
