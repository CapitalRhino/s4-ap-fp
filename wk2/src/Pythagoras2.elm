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
        [] ->
            []
        x :: xs ->
            if isTripleTuple x then
                List.singleton x ++ arePythTriplesRec xs
            else
                arePythTriplesRec xs