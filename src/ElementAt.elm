module ElementAt exposing (..)

import Browser
import Html exposing (Html, button, div, input, p, text)
import Html.Events exposing (onClick)



--Main


main =
    Browser.sandbox { init = init, update = update, view = view }



--Model


elementAt =
    2


type alias Model =
    { inputList : List String
    , iterationCounter : Int
    }


init : Model
init =
    { inputList = [ "One", "Two", "Three", "Four", "Five" ]
    , iterationCounter = 0
    }


type Msg
    = GetElementAt (List String)
    | IncrementIterationCounter


update : Msg -> Model -> Model
update msg model =
    case msg of
        GetElementAt inList ->
            { model
                | inputList = inList
                , iterationCounter = model.iterationCounter + 1
            }

        IncrementIterationCounter ->
            { model | iterationCounter = model.iterationCounter + 1 }


view : Model -> Html Msg
view model =
    div []
        [ div [] <|
            renderToList model.inputList model
        , div [] [ button [ onClick (GetElementAt model.inputList) ] [ text "Go" ] ]
        ]


renderToList : List String -> Model -> List (Html Msg)
renderToList xs model =
    case xs of
        [] ->
            [ div [] [ text "" ] ]

        item :: tail ->
            if elementAt == 2 then
                p [] [ text item ] :: renderToList [] model

            else
                p [] [] :: renderToList tail model


renderto list (somelist) {
      -- if list is empty 
           --return empty div[][]

    -- if elementAt Number == iterationCounter
        -- get the head of list
        -- return from function
}