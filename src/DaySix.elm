module DaySix exposing (day6, day6Input, day6Test, day6part1, day6part2, getChildren)

import Dict exposing (Dict)


day6 : Int -> String -> Maybe Int
day6 rounds input =
    let
        res : List Int
        res =
            String.split "," input
                |> List.filterMap String.toInt
                |> List.map (calculateOffspring rounds Dict.empty)
    in
    res
        |> List.sum
        |> Just


day6part1 : Maybe Int
day6part1 =
    day6 80 day6Input


day6part2 : Maybe Int
day6part2 =
    day6 256 day6Input


calculateOffspring : Int -> Dict ( Int, Int ) Int -> Int -> Int
calculateOffspring rounds dict remaining =
    let
        newRounds =
            rounds - (remaining + 1)

        children =
            getChildren rounds remaining
    in
    let
        childrenSum : Int
        childrenSum =
            List.range 0 (children - 1) |> List.map (\x -> calculateOffspring (newRounds - (7 * x)) dict 8) |> List.sum

        res =
            1 + childrenSum
    in
    res


getChildren : Int -> Int -> Int
getChildren rounds remaining =
    let
        newRounds =
            rounds - (remaining + 1)
    in
    if newRounds >= 0 then
        1 + (newRounds // 7)

    else
        0


day6Test : String
day6Test =
    "3,4,3,1,2"


day6Input : String
day6Input =
    "2,5,2,3,5,3,5,5,4,2,1,5,5,5,5,1,2,5,1,1,1,1,1,5,5,1,5,4,3,3,1,2,4,2,4,5,4,5,5,5,4,4,1,3,5,1,2,2,4,2,1,1,2,1,1,4,2,1,2,1,2,1,3,3,3,5,1,1,1,3,4,4,1,3,1,5,5,1,5,3,1,5,2,2,2,2,1,1,1,1,3,3,3,1,4,3,5,3,5,5,1,4,4,2,5,1,5,5,4,5,5,1,5,4,4,1,3,4,1,2,3,2,5,1,3,1,5,5,2,2,2,1,3,3,1,1,1,4,2,5,1,2,4,4,2,5,1,1,3,5,4,2,1,2,5,4,1,5,5,2,4,3,5,2,4,1,4,3,5,5,3,1,5,1,3,5,1,1,1,4,2,4,4,1,1,1,1,1,3,4,5,2,3,4,5,1,4,1,2,3,4,2,1,4,4,2,1,5,3,4,1,1,2,2,1,5,5,2,5,1,4,4,2,1,3,1,5,5,1,4,2,2,1,1,1,5,1,3,4,1,3,3,5,3,5,5,3,1,4,4,1,1,1,3,3,2,3,1,1,1,5,4,2,5,3,5,4,4,5,2,3,2,5,2,1,1,1,2,1,5,3,5,1,4,1,2,1,5,3,5,2,1,3,1,2,4,5,3,4,3"
