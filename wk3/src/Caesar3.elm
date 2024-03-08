module Caesar3 exposing (..)

-- importing Caesar cipher (part 2)
import Caesar2 exposing (..)

{-
    Caesar cipher (Part 3)
    Dimitar Byalkov and Kaloyan Stoykov
-}

candidates: List String -> String -> List (Int, String)
candidates canaries encryptedText =
    case canaries of
        [] ->
            []
        x :: xs ->
            filterList x (bruteGenerator encryptedText 0) ++ candidates (List.drop 1 canaries) encryptedText

bruteGenerator: String -> Int -> List (Int, String)
bruteGenerator encryptedText accumulator = 
    if accumulator < 25 then
        (accumulator, (decrypt accumulator encryptedText)) :: bruteGenerator encryptedText (accumulator + 1)
    else
        List.singleton (accumulator, (decrypt accumulator encryptedText))
            
filterList: String -> List (Int, String) -> List (Int, String)
filterList canary candidateTexts = 
    case candidateTexts of
    [] ->
        []
    (x, y) :: xs ->
        if containsText canary y then
            (x, y) :: filterList canary xs
        else
            filterList canary xs

containsText: String -> String -> Bool
containsText canary text =
    if String.left (String.length canary) text == canary then
        True
    else if String.length text == 0 then
        False
    else
        containsText canary (String.dropLeft 1 text)