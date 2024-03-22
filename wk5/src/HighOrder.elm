module HighOrder exposing (..)

{-
    Higher order functions
    Dimitar Byalkov and Kaloyan Stoykov
-}

repeatUntil: (a -> Bool) -> (a -> a) -> a -> a
repeatUntil p f init =
    let
        fResult = f init
    in
        if not(p fResult) then
            repeatUntil p f fResult
        else
            fResult

double: Int -> Int
double a = 
    a * 2

above100: Int -> Bool
above100 x = 
    x > 100

aboveN: Int -> (Int -> Bool)
aboveN n =
    ((<=) n)

log: Int -> Int -> Int
log base result = 
    if result == base then
        1
    else
        log base ((repeatUntil (aboveN result) (\x -> base * x) base) 
            // base) + 1

myCollatz: List Int -> List Int
myCollatz values = 
    case values of
        [] ->
            []
        x :: _ ->
            if modBy 2 x == 0 then
                x // 2 :: values
            else
                (3 * x + 1) :: values

myPredicate: List Int -> Bool
myPredicate vals =
    case vals of
        [] ->
            False
        x :: _ ->
            x == 1

-- Tests

testCollatzEven: Bool
testCollatzEven = repeatUntil myPredicate myCollatz [24] == 
    [1,2,4,8,16,5,10,3,6,12,24]

testCollatzOdd: Bool
testCollatzOdd = repeatUntil myPredicate myCollatz [19] == 
    [1,2,4,8,16,5,10,20,40,13,26,52,17,34,11,22,44,88,29,58,19]

testMyPredicate: Bool
testMyPredicate = myPredicate [1,2,4,8,16]

testDouble: Bool
testDouble = double 2 == 4

testAbove100: Bool
testAbove100 = above100 101


testRepeatUntil1: Bool
testRepeatUntil1 = repeatUntil above100 double 7 == 112

testRepeatUntil2: Bool
testRepeatUntil2 = repeatUntil above100 ((+) 1) 42 == 101

testRepeatUntil3: Bool
testRepeatUntil3 = repeatUntil above100 ((+) 10) 42 == 102

testRepeatUntil4: Bool
testRepeatUntil4 = repeatUntil above100 ((*) 2) 42 == 168


testLog100Base2: Bool
testLog100Base2 = log 2 100 == 7 -- 2⁷ = 128

testLog81Base3: Bool
testLog81Base3 = log 3 81 == 4 -- 3⁴ = 81%

testLog82Base3: Bool
testLog82Base3 = log 3 82 == 5 -- 3⁴ = 81

testLog80Base3: Bool
testLog80Base3 = log 3 80 == 4 -- 3⁴ = 81

testLog25Base5: Bool
testLog25Base5 = log 5 25 == 2

testLog48Base6: Bool
testLog48Base6 = log 6 48 == 3


allHighOrderTests: List Bool
allHighOrderTests = [testCollatzEven, testCollatzOdd, testMyPredicate, testDouble, 
    testAbove100, testRepeatUntil1, testRepeatUntil2, testRepeatUntil3, 
    testRepeatUntil4, testLog100Base2, testLog81Base3, testLog82Base3, testLog80Base3, testLog25Base5, testLog48Base6]

