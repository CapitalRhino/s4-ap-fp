module Sorting exposing (..)

{-
    Merge Sort
    Dimitar Byalkov and Kaloyan Stoykov
-}

msort: List comparable -> List comparable
msort myList =
    case myList of
        [] ->
            []
        [x] ->
            [x]
        _ ->
            let
                mid = List.length myList // 2
                left = List.take mid myList
                right = List.drop mid myList
            in
                merge (msort left) (msort right)          

merge: List comparable -> List comparable -> List comparable
merge listOne listTwo =
    case (listOne, listTwo) of
        ([], []) ->
            []
        (x, []) ->
            x
        ([], y) ->
            y
        (x :: xs, y :: ys) ->
            if x < y then
                x :: merge xs listTwo
            else
                y :: merge listOne ys

-- Tests
testMergeSortOdd: Bool
testMergeSortOdd = msort [9,8,4,5,0,8,2,3,4] == [0,2,3,4,4,5,8,8,9]

testMergeSortEven: Bool
testMergeSortEven = msort [2,3,0,9,4,8,0,4] == [0,0,2,3,4,4,8,9]

testMergeSortEmpty: Bool
testMergeSortEmpty = msort [] == []

testMergeSort1: Bool
testMergeSort1 = msort [6] == [6]

testMergeSort2: Bool
testMergeSort2 = msort [8,2] == [2,8]

testMergeSortPreSorted: Bool
testMergeSortPreSorted = msort [1,2,3,4,5] == [1,2,3,4,5]

testAllMergeSort: List Bool
testAllMergeSort = [testMergeSortOdd, testMergeSortEven, testMergeSortEmpty, testMergeSort1, testMergeSort2, testMergeSortPreSorted]