module Pythagoras2 exposing (..)

{-  
    Pythagoraen triples (Part 2)
    Dimitar Byalkov and Kaloyan Stoykov
-}

pythTriplesMap: List (Int, Int) -> List (Int, Int, Int)
pythTriplesMap input =
    List.map pythTriple input

pythTriplesRec: List (Int, Int) -> List (Int, Int, Int)
pythTriplesRec input = 
    -- We have a base case for an empty list which stops the recursion.
    case input of 
        [] ->
            []
        {- We take out the first element x of the list and 
           we are left with the rest of the list - xs, which we use to recursively call the function -}
        x :: xs ->
            List.singleton (pythTriple x) ++ pythTriplesRec xs


-- Taken and condensed from Pythagoras, part 1
pythTriple: (Int, Int) -> (Int, Int, Int)
pythTriple (x, y) =
    if x < 0 || y < 0 then
        (0, 0, 0)
    else
        ((x ^ 2 - y ^ 2), (2 * y * x), (x ^ 2 + y ^ 2))

{- 
    Taken from Pythagoras, part 1
    Checks if the three numbers form a Pythagorean triple
-}
isTriple: Int -> Int -> Int -> Bool
isTriple a b c =
    if a <= 0 || b <= 0 || c <= 0 then
        False
    else if (a ^ 2 + b ^ 2) /= c ^ 2 then
        False
    else True

-- check a give triple tuple forms a Pythagorean triple
isTripleTuple : (Int, Int, Int) -> Bool
isTripleTuple (a, b, c) = 
    isTriple a b c

arePythTriplesFilter: List (Int, Int, Int) -> List (Int, Int, Int)
arePythTriplesFilter tripleList =
    List.filter isTripleTuple tripleList



arePythTriplesRec: List (Int, Int, Int) -> List (Int, Int, Int)
arePythTriplesRec tripleList = 
    case tripleList of 
        -- Base Case
        [] ->
            []
        
        x :: xs ->
            -- Check if first element (x) is a triple tuple and call recursively for the rest of the list -> (x + y) :: arePythTriplesRec(rest of tuples list)
            if isTripleTuple x then
                List.singleton x ++ arePythTriplesRec xs

            -- Else - means x was not a triple tuple - call recursively for the rest of the list as parameter.
            else
                arePythTriplesRec xs


testPythTriplesMap: Bool
testPythTriplesMap = (pythTriplesMap [(5,4),(2,1),(35,7)] == [(9,40,41),(3,4,5),(1176,490,1274)])

testPythTriplesMapB: Bool
testPythTriplesMapB = (pythTriplesMap [(7,8), (9,7)] == [(-15,112,113),(32,126,130)])


testPythTriplesRec: Bool
testPythTriplesRec = (pythTriplesRec [(5,4),(2,1),(35,7)] == [(9,40,41),(3,4,5),(1176,490,1274)])


testPythTriplesRecB: Bool
testPythTriplesRecB = (pythTriplesRec [(6,4),(2,1),(20,7)] == [(20,48,52),(3,4,5),(351,280,449)]) 


testIsTripleZeros: Bool
testIsTripleZeros =  (isTriple 0 0 0 == False)

testIsTripleNotEqual: Bool
testIsTripleNotEqual = (isTriple 6 5 4 == False)

testIsTripleNotEqual2: Bool
testIsTripleNotEqual2 = (isTriple 4 6 5 == False)

testIsTripleEqual: Bool
testIsTripleEqual =(isTriple 3 4 5 == True)


testPythTriple: Bool
testPythTriple = (pythTriple (4, 5) == (-9, 40, 41))

testPythTripleB: Bool
testPythTripleB = (pythTriple (7, 3) == (40, 42, 58))

testisTripleTuple: Bool
testisTripleTuple = (isTripleTuple (3, 4, 5) == True)


testArePythTriplesFilter: Bool
testArePythTriplesFilter = (arePythTriplesFilter [(1,2,3), (9,40,41), (3,4,5), (100,2,500)]   == [(9,40,41),(3,4,5)] )


testArePythTriplesFilterB: Bool
testArePythTriplesFilterB = (arePythTriplesFilter [(1,2,3), (9,40,41), (3,4,5), (360,2,500)] == [(9,40,41),(3,4,5)] )


testArePythTriplesRec: Bool
testArePythTriplesRec = (arePythTriplesRec [(1,2,3), (9,40,41), (3,4,5), (100,2,500)]   == [(9,40,41),(3,4,5)] )

testArePythTriplesRecB: Bool
testArePythTriplesRecB = (arePythTriplesRec [(1,2,3), (9,40,41), (3,4,5), (360,2,500)] == [(9,40,41),(3,4,5)] )


allTests = [testPythTriplesMap, testPythTriplesMapB, testPythTriplesRec, testPythTriplesRecB, testIsTripleZeros, testIsTripleNotEqual, testIsTripleNotEqual2, testIsTripleEqual, testPythTriple, testPythTripleB, testisTripleTuple, testArePythTriplesFilter, testArePythTriplesFilterB, testArePythTriplesRec, testArePythTriplesRecB]
