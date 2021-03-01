module DictExample exposing (..)



main =
    Browser.sandbox { init = init, view = view, update = update }




type alias Model =
    { dictKeyValue : Dict String Boolean
    }



    init : Model
        init =
    { dictKeyValue = "Fish" False
    }

type Msg
    = doNothing
    

update : Msg -> Model -> Model
update msg model =
    case msg of
        doNothing ->
        model


view : Model -> Html Msg
view model =
    div [] [text "Success"]