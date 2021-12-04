module Main exposing (..)

import Browser
import DayFour
import DayThree
import Html exposing (Html, div, text)


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    Int


init : Model
init =
    0



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model



-- VIEW


view : Model -> Html Msg
view _ =
    print "4" DayFour.day4part1 DayThree.day3part2


print : String -> Maybe Int -> Maybe Int -> Html Msg
print day part1 part2 =
    div []
        [ text "Day "
        , text day
        , text " solution: "
        , text <|
            case part1 of
                Just value ->
                    String.fromInt value

                Nothing ->
                    "failed"
        , text " "
        , text <|
            case part2 of
                Just value ->
                    String.fromInt value

                Nothing ->
                    "failed"
        ]
