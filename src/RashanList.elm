module RashanList exposing (..)

import Browser
import Html exposing (Html, div, text)
import List exposing (..)
import Main exposing (Model, init, update, view)



--Main


main =
    Browser.sandbox { init = init, view = view, update = update }



-- Model


type alias Model =
    { shoppingList : List String }


init : Model
init =
    { shoppingList = [ "My", "name", "is", "Amit" ]
    }


type Msg
    = Additem String



-- Update


update : Msg -> Model -> Model
update msg model =
    case msg of
        Additem inputitem ->
            { model | shoppingList = inputitem :: shoppingList }



--View


view : Model -> Html Msg
view model =
    div [] [ text shoppingList ]
