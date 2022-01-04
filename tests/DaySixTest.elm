module DaySixTest exposing (..)

import DaySix
import Expect
import Test exposing (..)


suite : Test
suite =
    describe "skip, only, and todo"
        [ describe "test input"
            [ test "0" <|
                \_ -> DaySix.day6 0 DaySix.day6Test |> Expect.equal (Just 5)
            , test "1" <|
                \_ -> DaySix.day6 1 DaySix.day6Test |> Expect.equal (Just 5)
            , test "2" <|
                \_ -> DaySix.day6 2 DaySix.day6Test |> Expect.equal (Just 6)
            , test "3" <|
                \_ -> DaySix.day6 3 DaySix.day6Test |> Expect.equal (Just 7)
            , test "4" <|
                \_ -> DaySix.day6 4 DaySix.day6Test |> Expect.equal (Just 9)
            , test "5" <|
                \_ -> DaySix.day6 5 DaySix.day6Test |> Expect.equal (Just 10)
            , test "6" <|
                \_ -> DaySix.day6 6 DaySix.day6Test |> Expect.equal (Just 10)
            , test "7" <|
                \_ -> DaySix.day6 7 DaySix.day6Test |> Expect.equal (Just 10)
            , test "8" <|
                \_ -> DaySix.day6 8 DaySix.day6Test |> Expect.equal (Just 10)
            , test "9" <|
                \_ -> DaySix.day6 9 DaySix.day6Test |> Expect.equal (Just 11)
            , test "17" <|
                \_ -> DaySix.day6 17 DaySix.day6Test |> Expect.equal (Just 22)
            , test "18" <|
                \_ -> DaySix.day6 18 DaySix.day6Test |> Expect.equal (Just 26)
            , test "18 simple 3" <|
                \_ -> DaySix.day6 18 "3" |> Expect.equal (Just 5)
            , test "18 simple 4" <|
                \_ -> DaySix.day6 18 "4" |> Expect.equal (Just 4)
            , test "18 simple 1" <|
                \_ -> DaySix.day6 18 "1" |> Expect.equal (Just 7)
            , test "18 simple 2" <|
                \_ -> DaySix.day6 18 "2" |> Expect.equal (Just 5)
            , test "80" <|
                \_ -> DaySix.day6 80 DaySix.day6Test |> Expect.equal (Just 5934)
            , test "256" <|
                \_ -> DaySix.day6 256 DaySix.day6Test |> Expect.equal (Just 26984457539)
            , test "part1" <|
                \_ -> DaySix.day6 80 DaySix.day6Input |> Expect.equal (Just 350605)
            ]
        ]
