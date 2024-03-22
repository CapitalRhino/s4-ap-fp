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