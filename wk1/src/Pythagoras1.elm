module Pythagoras1 exposing (..)
{-
    Pythagoraen triples
    Dimitar Byalkov and Kaloyan Stoykov
-}

sqr: Int -> Int
sqr n =
    n * n

isTriple: Int -> Int -> Int -> Bool
isTriple a b c =
    if a <= 0 || b <= 0 || c <= 0 then
        False
    else if (sqr a + sqr b) /= sqr c then
        False
    else True

leg1 : Int -> Int -> Int
leg1 x y =
    sqr x - sqr y

leg2 : Int -> Int -> Int 
leg2 x y =
    2 * y * x

hyp : Int -> Int -> Int
hyp x y =
    sqr x + sqr y

pythTriple : (Int, Int) -> (Int, Int, Int)
pythTriple inputTuple =
    if Tuple.first inputTuple < 0 || Tuple.second inputTuple < 0 then
        (0, 0, 0)
    else
        (leg1 (Tuple.first inputTuple) (Tuple.second inputTuple), leg2 (Tuple.first inputTuple) (Tuple.second inputTuple), hyp (Tuple.first inputTuple) (Tuple.second inputTuple))

isTripleTuple : (Int, Int, Int) -> Bool
isTripleTuple (a, b, c) = 
    isTriple a b c

-- Tests

testSqr: Bool
testSqr = (sqr 2 == 4)

testSqrNegaviteNumber: Bool
testSqrNegaviteNumber = (sqr -1 == 1)

testIsTripleZeros: Bool
testIsTripleZeros =  (isTriple 0 0 0 == False)

testIsTripleNotEqual: Bool
testIsTripleNotEqual = (isTriple 6 5 4 == False)

testIsTripleNotEqual2: Bool
testIsTripleNotEqual2 = (isTriple 4 6 5 == False)

testIsTripleEqual: Bool
testIsTripleEqual =(isTriple 3 4 5 == True)

testLeg1: Bool
testLeg1 = (leg1 5 6 == -11)

testLeg1B: Bool
testLeg1B = (leg1 7 3 == 40)

testLeg2: Bool
testLeg2 = (leg2 3 9 == 54)

testLeg2B: Bool
testLeg2B = (leg2 2 7 == 28)

testHyp: Bool
testHyp = (hyp 5 6 == 61)

testHypB: Bool
testHypB = (hyp 7 3 == 58)

testInvalidPythTriple: Bool
testInvalidPythTriple = (pythTriple (-5, -9) == (0, 0, 0))

testPythTriple: Bool
testPythTriple = (pythTriple (4, 5) == (-9, 40, 41))

testPythTripleB: Bool
testPythTripleB = (pythTriple (7, 3) == (40, 42, 58))

testisTripleTuple: Bool
testisTripleTuple = (isTripleTuple (3, 4, 5) == True)

allPythTests: List Bool
allPythTests = [testSqr, testSqrNegaviteNumber, testIsTripleZeros, testIsTripleNotEqual, testIsTripleNotEqual2, testIsTripleEqual, testLeg1, testLeg1B, testLeg2, testLeg2B, testHyp, testHypB, testInvalidPythTriple, testPythTriple, testPythTripleB, testisTripleTuple]
