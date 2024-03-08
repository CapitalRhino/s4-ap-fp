module Caesar3 exposing (..)

-- importing Caesar cipher (part 2)
import Caesar2 exposing (..)

{-
    Caesar cipher (Part 3)
    Dimitar Byalkov and Kaloyan Stoykov
-}

-- Start value is 1, so the Brute Generator function can generate all possible decrypted strings
accumulatorStart: Int
accumulatorStart = 1

-- End value is 25 => the strings are decrypted with keys 1-25 
accumulatorEnd: Int
accumulatorEnd = 25

candidates: List String -> String -> List (Int, String)
candidates canaries encryptedText =
    case canaries of
        [] ->
            []
        x :: xs ->
            filterList x (bruteGenerator encryptedText accumulatorStart) ++ candidates (List.drop 1 canaries) encryptedText

-- The Brute Generator generates all possible decrypted strings from a given start value
bruteGenerator: String -> Int -> List (Int, String)
bruteGenerator encryptedText accumulator = 
    if accumulator < accumulatorEnd then
        (accumulator, (decrypt accumulator encryptedText)) :: bruteGenerator encryptedText (accumulator + 1)
    else
        List.singleton (accumulator, (decrypt accumulator encryptedText))

-- Filters a given list of tuples' second value by a given key (canary)
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
    -- Necessary to prevent a RangeError: Maximum call stack exceeded
    else if String.length text == 0 then
        False
    else
        containsText canary (String.dropLeft 1 text)

-- Tests
testContainsText: Bool
testContainsText =
    containsText "THE" "OVERTHELAZYDOG"

testContainsTextNot: Bool
testContainsTextNot =
    not (containsText "AND" "OVERTHELAZYDOG")

testBruteGenerator: Bool
testBruteGenerator = 
    bruteGenerator "AA" 1 == [(1,"ZZ"),(2,"YY"),(3,"XX"),(4,"WW"),(5,"VV"),(6,"UU"),(7,"TT"),(8,"SS"),(9,"RR"),(10,"QQ"),(11,"PP"),(12,"OO"),(13,"NN"),(14,"MM"),(15,"LL"),(16,"KK"),(17,"JJ"),(18,"II"),(19,"HH"),(20,"GG"),(21,"FF"),(22,"EE"),(23,"DD"),(24,"CC"),(25,"BB")]

testFilterList: Bool
testFilterList = 
    filterList "THE" (bruteGenerator "BPMYCQKSJZWEVNWF" 0) == [(8,"THEQUICKBROWNFOX")]

testCandidates: Bool
testCandidates =
    candidates ["THE", "AND"] "DGGADBCOOCZYMJHZYVMTOJOCZHVS" == [(5, "YBBVYWXJJXUTHECUTQHOJEJXUCQN"), (14,"PSSMPNOAAOLKYVTLKHYFAVAOLTHE"), (21,"ILLFIGHTTHEDROMEDARYTOTHEMAX")]

allCaesar3Tests: List Bool
allCaesar3Tests =
    [testContainsText, testContainsTextNot, testBruteGenerator, testFilterList, testCandidates]