module Main exposing (..)

import Browser
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onInput)



--MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



--MODEL


type alias Model =
    { content : String }


init : Model
init =
    { content = "" }



--UPDATE


type Msg
    = Change String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newContent ->
            { model
                | content = newContent
            }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "Voer een getal in", value model.content, onInput Change ] []
        , div [] [ text (show (String.toInt model.content)) ]
        ]


show : Maybe Int -> String
show int =
    case int of
        Nothing ->
            ""

        Just i ->
            intToEnglish i


intToEnglish : Int -> String
intToEnglish i =
    if i < 0 then
        "negative " ++ intToEnglish -i

    else if i >= 1000000000000 then
        "a lot"

    else if i >= 1000000000 then
        composeNumber i 1000000000 "billion" " "

    else if i >= 1000000 then
        composeNumber i 1000000 "million" " "

    else if i >= 1000 then
        composeNumber i 1000 "thousand" " "

    else if i >= 100 then
        composeNumber i 100 "hundred" " and "

    else if i >= 90 then
        composeNumber i 90 "ninety" "-"

    else if i >= 80 then
        composeNumber i 80 "eighty" "-"

    else if i >= 70 then
        composeNumber i 70 "seventy" "-"

    else if i >= 60 then
        composeNumber i 60 "sixty" "-"

    else if i >= 50 then
        composeNumber i 50 "fifty" "-"

    else if i >= 40 then
        composeNumber i 40 "forty" "-"

    else if i >= 30 then
        composeNumber i 30 "thirty" "-"

    else if i >= 20 then
        composeNumber i 20 "twenty" "-"

    else if i == 19 then
        "nineteen"

    else if i == 18 then
        "eighteen"

    else if i == 17 then
        "seventeen"

    else if i == 16 then
        "sixteen"

    else if i == 15 then
        "fifteen"

    else if i == 14 then
        "fourteen"

    else if i == 13 then
        "thirteen"

    else if i == 12 then
        "twelve"

    else if i == 11 then
        "eleven"

    else if i == 10 then
        "ten"

    else if i == 9 then
        "nine"

    else if i == 8 then
        "eight"

    else if i == 7 then
        "seven"

    else if i == 6 then
        "six"

    else if i == 5 then
        "five"

    else if i == 4 then
        "four"

    else if i == 3 then
        "three"

    else if i == 2 then
        "two"

    else if i == 1 then
        "one"

    else if i == 0 then
        "zero"

    else
        "unexpected error"


composeNumber : Int -> Int -> String -> String -> String
composeNumber i radix name separator =
    let
        suffix =
            intToEnglish (modBy radix i)

        fullSuffix =
            if String.length suffix /= 0 && modBy radix i /= 0 then
                separator ++ suffix

            else
                ""

        prefix =
            if i >= 100 then
                intToEnglish (i // radix) ++ " "

            else
                ""
    in
    prefix ++ name ++ fullSuffix
