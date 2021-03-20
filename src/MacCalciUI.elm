module MacCalciUI exposing (..)

import Browser
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


type alias Model =
    { theNumber : Int
    }


type Msg
    = UpdateTheNumber


init : Model
init =
    { theNumber = 56
    }



-- Update


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateTheNumber ->
            { model | theNumber = model.theNumber + 1 }



--view : Model -> Element Msg


view model =
    div [] [ row [] [] ]
