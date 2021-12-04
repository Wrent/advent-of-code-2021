module DayFour exposing (day4part1)

import Dict exposing (Dict)
import List.Extra


day4part1 : Maybe Int
day4part1 =
    String.split "\n\n" day4Input
        |> parseInput
        |> processInput
        |> Just


processInput : ( List Int, List BingoBoard ) -> Int
processInput input =
    let
        ( drawOrder, boards ) =
            input

        ( _, result ) =
            List.foldl draw ( boards, -1 ) drawOrder
    in
    result


draw : Int -> ( List BingoBoard, Int ) -> ( List BingoBoard, Int )
draw drawn input =
    let
        ( boards, result ) =
            input

        nextBoards =
            markDrawn drawn boards

        nextResult =
            getResult result nextBoards drawn
    in
    ( nextBoards, nextResult )


getResult : Int -> List BingoBoard -> Int -> Int
getResult oldResult boards drawn =
    if oldResult /= -1 then
        oldResult

    else
        let
            winningBoard =
                getWinningBoard boards
        in
        case winningBoard of
            Just board ->
                sumUnmarked board * drawn

            Nothing ->
                -1


getWinningBoard : List BingoBoard -> Maybe BingoBoard
getWinningBoard boards =
    List.Extra.find isWinning boards


isWinning : BingoBoard -> Bool
isWinning board =
    (Dict.values board |> List.any isWinningRow) || hasWinningCol board


hasWinningCol : BingoBoard -> Bool
hasWinningCol row =
    Dict.values row
        |> List.map Dict.values
        |> List.Extra.transpose
        |> List.map (List.indexedMap Tuple.pair)
        |> List.map Dict.fromList
        |> List.any isWinningRow


isWinningRow : BingoRow -> Bool
isWinningRow row =
    Dict.values row
        |> List.all (\b -> b.isMarked == True)


sumUnmarked : BingoBoard -> Int
sumUnmarked board =
    Dict.values board
        |> List.concatMap Dict.values
        |> List.filter (\b -> b.isMarked == False)
        |> List.map (\b -> b.value)
        |> List.sum


markDrawn : Int -> List BingoBoard -> List BingoBoard
markDrawn drawn boards =
    List.map (markBoard drawn) boards


markBoard : Int -> BingoBoard -> BingoBoard
markBoard drawn board =
    Dict.map (markRow drawn) board


markRow : Int -> Int -> BingoRow -> BingoRow
markRow drawn _ row =
    Dict.map (markField drawn) row


markField : Int -> Int -> BingoField -> BingoField
markField drawn _ field =
    if drawn == field.value then
        BingoField field.value True

    else
        field


parseInput : List String -> ( List Int, List BingoBoard )
parseInput input =
    ( parseDraw (List.head input |> Maybe.withDefault ""), parseBingoBoards (List.tail input |> Maybe.withDefault []) )


parseDraw : String -> List Int
parseDraw input =
    String.split "," input |> List.filterMap String.toInt


parseBingoBoards : List String -> List BingoBoard
parseBingoBoards input =
    List.map parseBingoBoard input


parseBingoBoard : String -> BingoBoard
parseBingoBoard input =
    String.split "\n" input
        |> List.map (\x -> String.split " " x |> List.map String.trim)
        |> List.map parseBingoRow
        |> List.indexedMap Tuple.pair
        |> Dict.fromList


parseBingoRow : List String -> BingoRow
parseBingoRow input =
    List.filterMap String.toInt input
        |> List.map initBingoField
        |> List.indexedMap Tuple.pair
        |> Dict.fromList


type alias BingoBoard =
    Dict Int BingoRow


type alias BingoRow =
    Dict Int BingoField


initBingoField : Int -> BingoField
initBingoField val =
    BingoField val False


type alias BingoField =
    { value : Int
    , isMarked : Bool
    }


day4Input : String
day4Input =
    "83,5,71,61,88,55,95,6,0,97,20,16,27,7,79,25,81,29,22,52,43,21,53,59,99,18,35,96,51,93,14,77,15,3,57,28,58,17,50,32,74,63,76,84,65,9,62,67,48,12,8,68,31,19,36,85,98,30,91,89,66,80,75,47,4,23,60,70,87,90,13,38,56,34,46,24,41,92,37,49,73,10,94,26,42,40,33,54,86,82,72,39,2,45,78,11,1,44,69,64\n\n97 62 17  5 79\n 1 99 98 80 84\n44 16  2 40 94\n68 95 49 32  8\n38 35 23 89  3\n\n48 53 59 99 43\n77 24 62 50 27\n28  8 10 86 18\n96  9 92 66 67\n20 55 87 52 31\n\n79 51 62 33  5\n15 39 21 48 90\n88 29  7 92 98\n87 49 84  6 14\n72 85 46 71 26\n\n 3 86 40 61 65\n 4 82 28 46 32\n31  5 33 96 98\n30 62 68 75 70\n 9 18 92 19 72\n\n82 24 95 21 79\n85 84 38 89 50\n 7 10  5 25 20\n99 37 48 86 12\n68 93  6 66 43\n\n 9 95 75 14  1\n94 90 40 84 24\n43 72 93  4 87\n48 50 53 20  6\n65 11 38 25 46\n\n41 22 47 34 55\n74 57 42 85 33\n40 21 52 78  7\n51 58 37  4 49\n53 75 11 48 76\n\n90  6 98 25 80\n41 81 30 87 33\n11 21 79 62 92\n27 60 46 56 88\n 4 69 70 13 84\n\n 1 22 72 43 58\n78 97 52 61 62\n27 48 81  2 63\n33 37  4 82 18\n65 28 70 31 59\n\n78 51 69 47 16\n48 55 58 70 37\n 7 59 66  5 76\n94 52 82 22 10\n13 83 95 24 79\n\n 8 38 40 67 24\n45  9 21  7 89\n82 96 72 92  4\n86 49 80 79 22\n26 11 84 78 70\n\n32 73  0 37 86\n78 42 13 30 53\n44 99 51 12 96\n45 57 63 34 58\n41 91  7 49 52\n\n 1 66  8  6  7\n47 96 25 77 72\n23 22 31 42 24\n52 27 53 51 99\n21 65 35 84  5\n\n49  1 79 39 82\n 7 96 13 33 85\n 3 53 32 12 50\n36 30 27 55 95\n16 24  2 66 77\n\n45 75 85 35 72\n99 25 91 68 28\n29 52  1 80 98\n62 46 63 22 44\n82 86 57 24 58\n\n70 19 79  7 24\n35 71 93 42 76\n17 88 62 25 12\n54  0 11 32 58\n38 64 29 75 80\n\n58 93 63 52 23\n77 60  1 38 87\n75 89 85 25 91\n64 39 96 49 66\n14 45 84 13 29\n\n85 10 21 33 80\n78 86 77 41 36\n98 58 53 82 72\n75 20 65  3 46\n52 16 74 45 99\n\n45 97 96 23 62\n79 59 60 87 64\n75  2 30 47 50\n85 81 56 11 38\n17 26 40  7 66\n\n94 99 67 88 82\n96  5 21 53 52\n41 15 49 35 89\n54 39 66 24 51\n 9  6 62 33 70\n\n33 89 48  4 20\n46 66 45 76  7\n12 77 43 60 15\n54 58 91 95 69\n11  8 32 31 18\n\n63 78 55  7 60\n95 14 38 10 45\n 3 16 72 53 37\n 1 89 70 75 44\n 5  6 66 13 46\n\n74 65 27 53 39\n67 66 76 13 31\n75 51 11 49 59\n18 12 71  9 89\n98 24 73 26 43\n\n90 21 75 77 97\n80 29 54 16 10\n55 98 65 19  7\n96 76 20 28 88\n94 83 91 26 86\n\n60 57 22 95 23\n81  4 34 36 14\n77  1 45 24 19\n33 88  8 28 74\n 2 17 37 32 94\n\n34 82 45 65 44\n70 89 95 20 79\n88 18 62 68 37\n85 17 54 86 69\n97 25 13 42 67\n\n70 30 59 94 86\n40 87 20 69 25\n46 44 41 17 79\n75 99  3 91  8\n71 39 73 88 37\n\n90 76 12 80 58\n60 45 35 10 33\n79 19 65 54 21\n63 51 77 15 92\n34 53  7 59 44\n\n40 14 68 43 37\n12 35 29 82 48\n47 28 97 44 93\n95 56 33 96 27\n38 85 88 49  6\n\n88 36 81 42 10\n85 99 29 70 86\n64 15 37 96 61\n66 76 87 17 62\n91 16 60 13 65\n\n45 71 66 80 69\n53 39 29 92 99\n23  0 72 36 52\n75 70 33  2 14\n22 77 21 26  3\n\n52 32 14 66 47\n53  7  9 69 11\n19 36 57 54 65\n17 26 76 51 42\n13  8 44 63 39\n\n23 84 34 35 19\n29 71 81 32 92\n22 49 54  6 56\n64 94 53 89  2\n74 68 11 13 47\n\n34 25 67 59 66\n68 27 69 91 33\n 4 56 46 99 21\n51 13 24 41 12\n90 65 19 26 55\n\n15 85  8 65 79\n95 51 39 75 96\n18 45 68 81 71\n67 28 21 61 20\n70 29 92 74 36\n\n25 75 23  2 38\n66 52 42 62 16\n93 63 78 31 65\n 0 91 77  4 14\n61 59 53 17 10\n\n16 95 72 67 17\n71  3 38 90 14\n34  8 55 49 33\n54 79 20 27 80\n96 31 18 70 61\n\n60 46  4 56 49\n 2 36  8 51 54\n71 82 97  1 18\n45 69 37  6 26\n85 61 27 92 77\n\n62 90 59 67 25\n41 45  7 91 17\n10 29 75 43 82\n12 78 95 37 32\n28 66 76  2 49\n\n26  6 49 44 74\n94 34 73 70 64\n14 91 23 88 31\n90 55 62 75 43\n 4  1 63 57 19\n\n 2 30 11 55 52\n51 92 73 54 96\n89 22 67 56 17\n49 50  9 95 45\n23 74 13 75  7\n\n 6 31 78 64 89\n76 13 83 56 34\n95 29 97 49 37\n66 77 74 73 90\n87 41 62 39 85\n\n51 80 38 15 44\n53 23 83 61 63\n27 33 79 40 32\n84  2 82 20 93\n72 92 48 39 98\n\n36 78 46 84 14\n56 53 51 92 89\n39 99 77 22 32\n65 38 42 76  7\n62 31  1 87 95\n\n74 99  6  4 20\n95 81 27 59 88\n63 69 30 25 87\n92 96 89 42 18\n11 77 91  8 46\n\n29 62 77  3 89\n54 12 55 44 34\n66 78 83 98 22\n17 10 67 82 75\n43 16 84 41 19\n\n67 24  9 89 48\n56  7 44 47 68\n12 38 35 54 14\n95 58 78 13 28\n97  5 37 99 42\n\n48 64 21 23 92\n29 99 75  2 53\n41 97 74 39 89\n66 63 22 45 73\n20 68 30 35 78\n\n76  3 47 40 72\n41  7 68  5 58\n12 32 81 62 93\n91 80 17 78 61\n22 95 94 38 33\n\n42 27 70 13  5\n77 38 50  3 44\n29 56 36 15 97\n68 20 94 12 54\n64 83 25 55 80\n\n77 63 37 68 73\n34 30 22 91 10\n16 80 89 98 45\n46 36 90 95 83\n54 52 57 61 55\n\n55  3 33 66 69\n51 97 36 57 50\n56 74 35 84 44\n45 92 18 42 52\n85 13 27 70 20\n\n56 68 71 11 63\n12 93 57 94 84\n91 13 29 31 75\n54 49 51 73  5\n81  7 60 53 89\n\n73 55 87 35 84\n37 63 41 54 39\n58 42 85 66 68\n96 24 86 72 27\n40 28  4 80 33\n\n29 79  8 76 31\n30 20 12  0 61\n14 37 49 45 74\n64 17  1 91 51\n87 67  3 77 47\n\n72 15 46 71 75\n41 16 68 14 43\n97 25 78 26 39\n59 57 88  4 52\n20 49  3 23 29\n\n33 78 31 35  6\n85 43  7 87 18\n68 93  4 80 96\n98 13 61 77 23\n10 29 34 36  5\n\n 0 78 44 49 14\n72 88 30 31 81\n34 87 55 27 11\n58 64 76 40 62\n47 18 38 35 26\n\n16  2 67 56 74\n50 41 86 38 39\n32 96 59 40  8\n17 82 49 55 89\n34 88 81 73 94\n\n52 18 32 56 61\n40  5 48 64 62\n22 57 19 26 91\n31  3 95 27 87\n74 83 75 99 73\n\n 6 65 91 22 86\n82 72 60 41 87\n 2 71  9 12 84\n51 90 43 49 80\n15 20 54 66 29\n\n39 64 35 23 10\n73 25  1 45 93\n50 37 95 86 78\n52  6  2  0 13\n26 89 27 62 80\n\n65 67 95 33 60\n55 49 64 92  7\n56 75 73 35 99\n 8 72 80  0 46\n41 25  2 69  4\n\n26 51 31 44 25\n21  6 70 12 71\n67 69 13 63 79\n81 74  8 89 30\n16 48 88 72 66\n\n99 69 61 29 86\n67 88  5 20  2\n70 60 27 82  6\n95 65 30  9 85\n23 58 59 87 66\n\n40 90 43 57 26\n10 52 27 64 72\n 3 83 11 54 42\n39 20 87 15 81\n49 28 58 33 29\n\n11 32 63 96 81\n77 82  0 30 15\n88 31 41 46  6\n17 55 76 42 87\n24 93 70 66 40\n\n35  6 28 90 21\n72 74 78 43  3\n47 17 13 41 96\n68 12 76 81 11\n70 34 33 25 54\n\n94  9 58 91 38\n84  7 22 30 63\n23 26 49 93 48\n79 75 99 96 67\n90 19 66 57 47\n\n35 98 24 31 41\n79 63 92 70 11\n36  3 72 50 93\n90 21 40 38 77\n 0 14 42 99 67\n\n96 45 75 97 94\n68 35  9 30 67\n25 88 40 46 37\n82 79 90 76 55\n50 59 58 22 21\n\n96 73 49 36 56\n 6 45 30 81 76\n10 95 70 88 98\n43 47 74 66 84\n77 83 68 54 28\n\n96 48 64 89  6\n76 12 47  8 30\n39 55 95 11 62\n68 25 50 63 31\n59 17 46 52 78\n\n66 27 61 79 73\n37 88 47 84 72\n50 18 99  7 76\n97 11 53 43 30\n42 56 98 39 63\n\n64 13 45  7 72\n66 35 18 68 86\n38 30 89 11 29\n37 76 23 14 67\n36 61 87 26 46\n\n20 72 10 30 17\n25 14 74 71 58\n34 51 45 43 76\n38 75 50 98 42\n 2 12 67 66 82\n\n44 23 73 56 88\n 4 96 90  0 32\n40 86 47 87 50\n28 30 42 39 17\n10 12 16  8 14\n\n21 33  7 20 78\n81 46 77 42 79\n84 28 82 93 68\n90 63 60  0 34\n35 70 40 29 54\n\n93  8 11  2 39\n74 40 95 69 57\n86 21 31 88 63\n52 16 19 20 22\n72  7 25 90 77\n\n83 29 90 48 46\n97 21  2 65 15\n89 28 60 69 26\n77 75  9 35 96\n82 49 66  5 16\n\n80 57  2 73 46\n22 50 87 60 89\n95 74 98 93 62\n86 61 10 69  9\n48 31 53 88 84\n\n46 17 28 56 50\n64 65 43 73 22\n32 31 89 20 38\n13 49 18 55 72\n83 41 78 94 57\n\n39  8 68 87 21\n78 59 27  0 14\n25  3 96 51 63\n92 35 19 57 99\n83 75 69 37 72\n\n42 36 34 77 69\n21 55 47 52 89\n61 90  3 23 41\n45 80 29 27 99\n79 86 87 93 74\n\n59  8 97 48 73\n40 31 29 49 85\n41 68 11  9 45\n87 74 77 75 91\n67 27 70 90 16\n\n80 47 53 81 36\n75 35 87 90 89\n19  5 56 28 26\n 8 44 77 31 20\n61 96 27 99 79\n\n35 16 40 94 65\n60 28 46 51 61\n45 53 36 89 80\n33 93 12 39 42\n13 68 57 64 26\n\n39 55 88 78 72\n 6 82 52  1 60\n41 23 97 44 11\n 3 15 21 93 38\n24 90  7 80  2\n\n81 46 31 56 30\n94 22 58 69 41\n42 91 20  0 14\n71 11 17 37 12\n 7 73 79  9 26\n\n38 32 24 98 79\n48 49  4 17 90\n12 20 95 99 10\n94 23 30 92 97\n84 18 57 11 53\n\n75 22 42 59 55\n23 33 90  2 52\n94 13 78  0 16\n39 72 67 45 31\n11 53  7 83 28\n\n43 33 52 89 40\n53 94 87 90 19\n98 51 64 63 62\n66 65 57 93 18\n80 79 59 99 73\n\n57 63 96  3 27\n88 74  9 60 99\n48 30  1 18 15\n23 77 89 24 55\n37 58 67 91 10\n\n36 73 27 72  8\n75 74 87 55  7\n 2 67 34 84 51\n94 18 23 62 11\n65 41  3 29 53\n\n63 67 73 53 13\n28 54 19 72 93\n48 41 55 64 33\n83 70 65 26 22\n11 86 35 16 18\n\n13 50 19 48 58\n28 42 83 20 29\n 5 96 92 90  3\n87 93 56 23 78\n98 57  0 72 62\n\n95 76 16  5 56\n55 28 52 88 73\n 6 99 75 90 18\n12 25 22 44 57\n62 37 36 30 48\n\n24 41 73 90 46\n55 91 63 86 44\n 0 74 72 47 76\n34 13 33 65 62\n49 75 10 15 27\n\n85 63 62 11 38\n53 29  2  8 13\n87 64 31 69 58\n88 84 17  3 26\n 5 32 23 33 39\n\n25  8 81 29 95\n65 56 86 34 17\n38 66 85 43 26\n39 12 70 32 19\n49 68 10  4 13"
