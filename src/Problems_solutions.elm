module Main exposing (isPalindrome, lastElement, listLength, listReverse, ls, main, printAList, secondLastElement, stringFromBool)

import Html exposing (text)


main =
    --case elementAt ls of
    --Nothing -> text "Nothing"
    --Just a -> text a
    --text (String.fromInt (listLength ls))
    --text (stringFromBool(isPalindrome ["One","Two","Two", "One"] ))
    --text (String.fromList (listReverse ls))
    text (printAList (listReverse ls))



--text(stringFromBool(isPalindrome ls))


ls =
    [ "One", "Two", "Three", "Four", "Five", "Six" ]



--ls = ["One", "Two", "Two", "One"]


isPalindrome : List String -> Bool
isPalindrome xs =
    xs == listReverse xs


listReverse : List String -> List String
listReverse xs =
    case xs of
        [] ->
            []

        x :: tail ->
            listReverse tail ++ [ x ]


stringFromBool : Bool -> String
stringFromBool value =
    if value then
        "True"

    else
        "False"


printAList : List String -> String
printAList xs =
    case xs of
        [] ->
            " Nothing is in the list "

        [ x ] ->
            x

        x :: tail ->
            x ++ " " ++ printAList tail


lastElement : List a -> Maybe a
lastElement xs =
    case xs of
        [] ->
            Nothing

        [ x ] ->
            Just x

        x :: tail ->
            lastElement tail


secondLastElement : List a -> Maybe a
secondLastElement xs =
    case xs of
        [] ->
            Nothing

        [ x, u ] ->
            Just x

        head :: tail ->
            lastElement tail


listLength : List a -> Int
listLength xs =
    case xs of
        [] ->
            0

        head :: tail ->
            1 + listLength tail



--isPalindrome : List a -> Bool
--isPalindrome xs =
--case xs of
--[] -> True
--[x] -> True
--[x, u] -> x==u
--x :: tail -> isPalindrome (x:: (Maybe.withDefault "" (List.head (List.reverse tail)))
