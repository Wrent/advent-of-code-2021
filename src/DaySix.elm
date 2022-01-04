module DaySix exposing (day6, day6Input, day6Test, day6part1, day6part2)


day6 : Int -> String -> Maybe Int
day6 rounds input =
    let
        vector : List Int
        vector =
            String.split "," input
                |> List.filterMap String.toInt
                |> parseVector

        _ =
            Debug.log "vector" vector
    in
    List.range 1 rounds
        |> List.foldl processRound vector
        |> getResult
        |> Just


parseVector : List Int -> List Int
parseVector list =
    List.range 0 8
        |> List.map (countOccurrences list)


countOccurrences : List Int -> Int -> Int
countOccurrences list x =
    List.filter (\a -> a == x) list |> List.length


processRound : Int -> List Int -> List Int
processRound _ vector =
    case vector of
        zero :: one :: two :: three :: four :: five :: six :: seven :: eight ->
            one :: two :: three :: four :: five :: six :: seven + zero :: eight ++ [ zero ]

        _ ->
            []


getResult : List Int -> Int
getResult vector =
    List.sum vector


day6part1 : Maybe Int
day6part1 =
    day6 80 day6Input


day6part2 : Maybe Int
day6part2 =
    day6 256 day6Input


day6Test : String
day6Test =
    "3,4,3,1,2"


day6Input : String
day6Input =
    "2,5,2,3,5,3,5,5,4,2,1,5,5,5,5,1,2,5,1,1,1,1,1,5,5,1,5,4,3,3,1,2,4,2,4,5,4,5,5,5,4,4,1,3,5,1,2,2,4,2,1,1,2,1,1,4,2,1,2,1,2,1,3,3,3,5,1,1,1,3,4,4,1,3,1,5,5,1,5,3,1,5,2,2,2,2,1,1,1,1,3,3,3,1,4,3,5,3,5,5,1,4,4,2,5,1,5,5,4,5,5,1,5,4,4,1,3,4,1,2,3,2,5,1,3,1,5,5,2,2,2,1,3,3,1,1,1,4,2,5,1,2,4,4,2,5,1,1,3,5,4,2,1,2,5,4,1,5,5,2,4,3,5,2,4,1,4,3,5,5,3,1,5,1,3,5,1,1,1,4,2,4,4,1,1,1,1,1,3,4,5,2,3,4,5,1,4,1,2,3,4,2,1,4,4,2,1,5,3,4,1,1,2,2,1,5,5,2,5,1,4,4,2,1,3,1,5,5,1,4,2,2,1,1,1,5,1,3,4,1,3,3,5,3,5,5,3,1,4,4,1,1,1,3,3,2,3,1,1,1,5,4,2,5,3,5,4,4,5,2,3,2,5,2,1,1,1,2,1,5,3,5,1,4,1,2,1,5,3,5,2,1,3,1,2,4,5,3,4,3"
