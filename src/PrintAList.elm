module PrintAList exposing (..)

import Browser
import Html exposing (Html, button, div, input, p, text)



--Main


main =
    Browser.sandbox { init = init, update = update, view = view }



--Model


type alias Model =
    { inputList : List String
    }


init : Model
init =
    { inputList = [ "One", "Two", "Three", "Four", "Five" ]
    }


type Msg
    = GetElementAt (List String)


update : Msg -> Model -> Model
update msg model =
    case msg of
        GetElementAt inList ->
            { model | inputList = inList }


view : Model -> Html Msg
view model =
    div [] (renderToList model.inputList)


renderToList xs =
    case xs of
        [] ->
            [ div [] [ text "" ] ]

        item :: tail ->
            p [] [ text item ] :: renderToList tail
