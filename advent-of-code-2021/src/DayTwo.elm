module DayTwo exposing (day2part1)


day2part1 : Maybe Int
day2part1 =
    String.split "\n" day2Input
        |> List.filterMap parseCommand
        |> List.foldl applyCommand initialSubmarinePosition
        |> getResult
        |> Just


getResult : SubmarinePosition -> Int
getResult pos =
    pos.horizontal * pos.depth



--
--
--day1part2 : Maybe Int
--day1part2 =
--    day1 listToNeighbouringWindows


parseCommand : String -> Maybe Command
parseCommand input =
    let
        split : List String
        split =
            String.split " " input

        text : Maybe String
        text =
            List.head split

        steps : Maybe Int
        steps =
            List.tail split |> Maybe.andThen List.head |> Maybe.andThen String.toInt
    in
    case ( text, steps ) of
        ( Just a, Just b ) ->
            case a of
                "forward" ->
                    Just <| Forward b

                "down" ->
                    Just <| Down b

                "up" ->
                    Just <| Up b

                _ ->
                    Nothing

        _ ->
            Nothing


applyCommand : Command -> SubmarinePosition -> SubmarinePosition
applyCommand command position =
    case command of
        Forward steps ->
            SubmarinePosition position.depth (position.horizontal + steps)

        Down steps ->
            SubmarinePosition (position.depth + steps) position.horizontal

        Up steps ->
            SubmarinePosition (position.depth - steps) position.horizontal


type Command
    = Forward Int
    | Down Int
    | Up Int


type alias SubmarinePosition =
    { depth : Int
    , horizontal : Int
    }


initialSubmarinePosition : SubmarinePosition
initialSubmarinePosition =
    SubmarinePosition 0 0


day2Input : String
day2Input =
    "forward 1\ndown 9\ndown 4\nforward 4\ndown 2\ndown 7\nup 9\ndown 5\nforward 9\nforward 9\ndown 2\nforward 7\nforward 9\ndown 5\ndown 2\nup 9\ndown 7\nforward 6\ndown 6\ndown 6\nup 1\ndown 2\nforward 8\nforward 8\nforward 9\nup 1\nforward 4\nup 1\nforward 3\nforward 5\nup 3\nup 7\ndown 4\ndown 1\nforward 3\nforward 4\nup 3\nforward 9\ndown 2\nforward 5\nup 2\nup 1\nup 5\nforward 1\nup 9\nforward 6\nup 4\ndown 4\nforward 3\nup 7\ndown 1\ndown 1\ndown 8\nforward 3\nforward 1\nup 7\nup 1\ndown 4\nup 6\ndown 6\nforward 8\ndown 3\ndown 9\ndown 6\nup 8\nforward 8\ndown 9\nup 7\ndown 7\nforward 8\ndown 8\nup 6\ndown 7\ndown 4\nup 7\nforward 7\nforward 7\nforward 2\nforward 5\ndown 5\ndown 9\ndown 8\ndown 5\nup 3\ndown 3\ndown 6\ndown 6\nforward 1\ndown 2\nforward 2\ndown 5\nup 2\nforward 5\ndown 1\ndown 3\nforward 4\ndown 2\nforward 8\ndown 6\nforward 3\nforward 3\nforward 9\ndown 9\ndown 4\ndown 8\ndown 7\ndown 3\nup 6\ndown 7\nforward 9\nup 9\ndown 8\nforward 9\nforward 8\ndown 2\nup 4\nforward 8\nup 7\ndown 2\nforward 6\nforward 7\ndown 5\nup 4\ndown 3\ndown 3\ndown 5\ndown 2\ndown 5\nforward 8\nforward 8\ndown 5\nup 4\ndown 6\nforward 5\nup 6\nup 5\ndown 4\nforward 7\nforward 5\ndown 7\nup 9\nup 4\nup 9\nforward 8\ndown 8\ndown 8\ndown 2\ndown 8\ndown 8\nup 6\nforward 2\ndown 9\nforward 5\nforward 4\ndown 8\nup 5\nforward 3\nforward 1\ndown 6\nforward 9\nforward 2\nforward 1\nup 5\ndown 2\nforward 5\nup 6\nup 6\nforward 9\nup 3\nforward 2\nforward 9\ndown 9\ndown 5\nup 4\nup 7\nup 8\nforward 4\nforward 3\ndown 7\ndown 5\nforward 5\ndown 6\nforward 9\nup 4\nup 6\ndown 2\nforward 6\nforward 7\nup 9\nup 5\ndown 4\nup 5\ndown 2\nforward 4\nup 9\ndown 1\nforward 7\ndown 9\nforward 5\ndown 5\ndown 2\nup 4\nup 8\ndown 8\ndown 7\nup 7\ndown 1\nforward 5\nforward 8\ndown 2\ndown 1\nforward 8\ndown 7\nup 6\nforward 9\nforward 4\ndown 8\nforward 1\nup 8\nforward 3\nup 4\nforward 9\ndown 6\nforward 5\nforward 1\nforward 8\nforward 5\nforward 8\ndown 4\nforward 8\nforward 2\nforward 8\nforward 3\ndown 3\nforward 6\nup 6\nup 9\ndown 4\ndown 6\nforward 6\ndown 9\nup 2\ndown 8\ndown 6\ndown 1\nforward 1\ndown 2\ndown 3\nforward 5\nforward 6\ndown 5\nup 7\nup 2\nforward 2\ndown 4\ndown 4\nforward 3\ndown 4\nup 4\nforward 1\ndown 4\nforward 9\nforward 7\nforward 6\ndown 3\ndown 4\nforward 3\nforward 3\nforward 3\nup 6\nforward 4\nforward 7\nforward 3\nforward 5\nforward 7\nforward 4\ndown 1\nup 2\nup 7\nforward 4\ndown 2\nforward 2\nup 7\ndown 7\nforward 5\nforward 9\ndown 8\nforward 5\nforward 5\ndown 9\nforward 1\nforward 6\ndown 2\ndown 3\ndown 1\ndown 7\ndown 3\nup 3\ndown 3\nforward 5\ndown 4\nforward 6\nforward 2\ndown 4\nforward 4\nup 5\nup 3\nforward 2\nforward 5\ndown 7\nup 9\nup 5\ndown 3\nforward 8\nforward 4\ndown 8\nup 4\nforward 8\nforward 7\nforward 1\ndown 2\ndown 6\nup 7\nforward 9\ndown 2\ndown 4\ndown 3\ndown 8\nup 4\ndown 2\nup 3\nforward 1\ndown 2\nup 6\ndown 9\nup 3\ndown 1\ndown 1\nforward 4\ndown 2\nforward 3\nforward 5\nforward 7\ndown 7\nup 3\nup 5\ndown 1\ndown 2\nforward 4\ndown 1\ndown 1\ndown 7\nup 4\nforward 3\ndown 3\nforward 2\ndown 3\nup 7\ndown 1\nup 5\ndown 7\nforward 4\nup 2\nforward 5\nup 4\ndown 4\ndown 3\nforward 1\nup 9\ndown 5\nforward 9\nforward 1\ndown 7\ndown 2\ndown 4\ndown 2\nforward 3\ndown 1\ndown 2\nup 9\nforward 2\ndown 8\nforward 1\nforward 3\ndown 5\ndown 4\nforward 2\nforward 8\nforward 9\ndown 9\ndown 6\nup 3\nforward 1\ndown 6\ndown 7\nup 3\nforward 9\ndown 2\nup 4\nforward 3\nforward 6\nforward 6\ndown 7\nforward 7\nforward 6\ndown 7\nup 9\nforward 7\ndown 1\ndown 8\ndown 2\ndown 2\ndown 9\ndown 6\nforward 1\ndown 9\nforward 9\nforward 8\nforward 6\nforward 9\ndown 5\nforward 9\nforward 4\nforward 4\ndown 6\nforward 6\nforward 5\nforward 7\ndown 7\ndown 9\nforward 4\ndown 9\nup 5\nforward 6\ndown 6\nforward 9\nup 7\nforward 6\nup 6\nup 9\nforward 9\nup 4\ndown 3\nup 9\ndown 8\ndown 2\nforward 4\ndown 8\ndown 2\nforward 2\ndown 5\nup 3\ndown 1\nforward 4\ndown 3\nforward 3\ndown 9\nforward 6\nforward 6\ndown 9\nforward 6\nforward 1\ndown 5\nup 1\nforward 3\nforward 1\nforward 5\ndown 8\nforward 7\nforward 2\nup 3\nforward 8\nforward 1\nforward 7\ndown 1\ndown 4\nforward 7\nforward 9\nforward 4\ndown 2\nforward 5\nforward 3\nforward 9\nforward 3\nup 3\nforward 9\ndown 3\nforward 9\nforward 3\nforward 7\ndown 5\nforward 6\ndown 6\nforward 5\nforward 1\ndown 1\nforward 8\ndown 3\nup 1\ndown 6\nforward 7\nup 5\nup 7\ndown 3\nforward 8\nup 8\ndown 4\nup 6\nup 7\nforward 1\nforward 2\nforward 7\ndown 1\nforward 4\ndown 8\nup 6\nup 7\nup 4\ndown 7\ndown 8\ndown 6\nforward 5\nforward 9\ndown 7\nforward 8\ndown 9\ndown 9\nforward 5\nforward 2\nup 4\ndown 2\nforward 3\ndown 2\nforward 1\ndown 7\ndown 6\nforward 5\nup 2\nforward 4\ndown 4\nforward 5\nforward 7\nup 5\ndown 7\nforward 9\nforward 3\nforward 6\nup 8\ndown 9\nforward 1\nup 8\nforward 3\ndown 6\ndown 1\ndown 3\ndown 4\nup 2\nup 3\nforward 8\nup 7\ndown 2\nforward 1\ndown 6\nforward 1\ndown 8\nforward 3\nforward 5\nforward 5\nup 5\nforward 4\nforward 6\ndown 8\nup 7\nforward 8\nup 6\nforward 5\ndown 4\nforward 8\ndown 3\nforward 7\nforward 9\nforward 8\nup 9\nforward 4\nup 5\nup 8\ndown 8\nforward 1\ndown 1\nforward 3\nforward 9\ndown 9\nup 2\nforward 5\ndown 1\ndown 6\nforward 2\nforward 2\ndown 2\ndown 8\nforward 5\nup 2\nup 2\nforward 3\ndown 6\nup 2\ndown 2\ndown 1\nup 9\nforward 6\nup 4\nforward 4\ndown 2\nforward 8\nup 9\ndown 1\nforward 9\nup 3\nup 7\nforward 5\ndown 9\ndown 2\ndown 5\nup 4\nup 8\nup 2\ndown 1\nup 4\nforward 8\nforward 2\ndown 8\nforward 9\nforward 1\ndown 5\nforward 3\ndown 6\nforward 3\nup 2\ndown 9\ndown 6\ndown 4\ndown 9\ndown 8\nforward 2\nforward 7\nup 4\nforward 8\nup 9\nup 6\nforward 2\ndown 1\ndown 5\ndown 5\ndown 3\nup 9\nup 9\nforward 9\nforward 5\ndown 1\ndown 3\ndown 8\ndown 4\nforward 5\nup 4\ndown 2\ndown 5\ndown 4\ndown 8\ndown 2\nforward 9\nforward 7\nup 9\nforward 8\ndown 9\ndown 2\nforward 5\ndown 9\nforward 7\ndown 2\ndown 9\nforward 4\nup 7\ndown 9\nforward 6\nforward 2\ndown 1\ndown 9\ndown 3\ndown 7\ndown 8\ndown 5\ndown 9\ndown 1\nup 9\ndown 7\nforward 7\nup 4\ndown 2\ndown 4\nforward 6\nforward 8\nforward 1\nforward 2\nup 2\nup 2\nforward 9\ndown 9\nforward 8\nforward 6\nforward 5\ndown 2\nforward 7\nup 3\nup 8\nforward 1\nforward 5\ndown 7\ndown 7\nforward 5\ndown 1\nup 3\nup 8\nforward 5\nforward 7\nforward 3\ndown 6\nup 7\nforward 8\ndown 5\nup 3\nforward 7\ndown 2\ndown 1\ndown 1\nforward 1\ndown 1\ndown 8\ndown 3\nup 1\nforward 8\nforward 9\nforward 5\nforward 8\ndown 2\nforward 3\ndown 5\nup 9\ndown 9\ndown 5\nforward 1\nup 9\nup 4\nup 7\nup 9\nup 4\ndown 2\nforward 6\nforward 4\nup 5\nup 6\ndown 8\ndown 6\ndown 6\nup 3\nforward 3\ndown 9\ndown 5\ndown 5\nforward 7\ndown 6\nforward 7\ndown 9\ndown 8\ndown 3\ndown 7\ndown 4\ndown 1\nforward 6\nup 7\ndown 1\nforward 7\ndown 2\nforward 4\ndown 3\nforward 6\nup 7\nforward 3\ndown 3\nup 6\ndown 2\ndown 4\ndown 8\nforward 6\ndown 4\nforward 7\ndown 2\nforward 9\ndown 3\ndown 6\ndown 3\ndown 6\nforward 4\ndown 8\nup 5\ndown 6\nforward 6\ndown 6\nforward 9\nup 8\ndown 7\nup 1\nforward 4\nup 3\nforward 9\nup 6\nforward 4\nup 8\nforward 2\ndown 7\ndown 7\nforward 1\nup 9\nup 7\nforward 3\ndown 1\nup 5\nforward 3\nforward 1\nforward 3\nforward 7\ndown 6\ndown 1\nforward 9\nup 1\nup 1\ndown 8\ndown 7\nforward 1\nup 8\nup 6\ndown 8\nforward 1\nup 3\ndown 7\nforward 7\ndown 7\nforward 4\nforward 6\nup 2\ndown 9\nforward 9\ndown 4\nup 2\nforward 7\nforward 4\nup 8\nup 5\ndown 8\nforward 7\nforward 6\ndown 1\nup 5\nup 5\ndown 3\nforward 7\nforward 1\nforward 7\nforward 1\ndown 3\nforward 3\nforward 4\nforward 6\ndown 3\nup 4\nup 5\ndown 8\nup 3\nforward 4\nforward 6\nforward 8\nforward 4\nforward 7\ndown 9\nup 3\ndown 1\nup 4\nforward 8\nforward 5\nforward 4\nforward 1\ndown 2\ndown 7\ndown 6\nforward 4\nforward 2\nup 2\ndown 2\nforward 2\nforward 2\nup 6\nforward 6\nforward 1\nforward 8\nup 7\nforward 4\nforward 2\ndown 2\ndown 1\nforward 4\ndown 4\nup 7\nup 2\nup 3\ndown 5\nforward 2\nforward 9\ndown 2\ndown 9\ndown 4\ndown 8\nup 9\nup 5\nup 2\nforward 6\ndown 6\nforward 9\nforward 7\nforward 8\nforward 4\nup 9\nforward 9\ndown 8\ndown 2\nforward 5\ndown 8\ndown 8\nforward 8\nforward 6\nforward 7\ndown 8\nforward 1\nforward 6\ndown 4\nforward 1\ndown 3\nforward 1\ndown 8\nforward 2\ndown 4\ndown 6\ndown 4\nup 9\nforward 6\nup 3\ndown 7\nforward 3\ndown 8\nforward 6\nforward 3\ndown 1\nforward 1\nup 6\ndown 1\ndown 2\nforward 5\ndown 3\nup 5\nforward 6\nforward 7\nforward 2\nforward 9\ndown 7\nup 4\nforward 6\nforward 8\ndown 6\ndown 7\ndown 4\ndown 5\ndown 6\ndown 3\nup 1\nforward 2\nforward 3\ndown 1\ndown 7\nup 8\nforward 1\nforward 6\nup 3\ndown 1\nup 6\ndown 2\nup 1\nup 7\nforward 3\nup 8\nforward 7\nforward 5\nforward 5"
