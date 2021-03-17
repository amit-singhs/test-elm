module LastElemen exposing (getLastElement, main, xs)

import GroceryList exposing (Msg(..))
import Html exposing (text)
import List


xs =
    [ "One", "Two", "Three", "Four", "Five", "Six" ]



--[ 1, 2, 3, 4, 5, 6 ]


main =
    --text (String.fromInt (getLastElement (List.reverse xs)))
    text (Maybe.withDefault "" (getLastElement xs))



--getLastElement : List Int -> Int


getLastElement : List String -> Maybe String
getLastElement ys =
    case List.reverse ys of
        [] ->
            Nothing

        --0
        head :: tail ->
            Just head
