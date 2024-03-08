module CreditCard exposing (..)
import Html exposing (Html)

------------------------------------------------------------------------------------------------------------------------------
-- Validating Credit Card Numbers
------------------------------------------------------------------------------------------------------------------------------

-- ===================================
-- Ex. 0
-- ===================================

toDigits: String -> List Int
toDigits x = 
    case String.uncons (String.filter Char.isDigit x) of
        Nothing ->
            []
        Just (head, tail) ->
            Maybe.withDefault 0 (String.toInt (String.fromChar head)) :: toDigits tail

-- ===================================
-- Ex. 1
-- ===================================

toDigitsRev: String -> List Int
toDigitsRev x = 
    List.foldl (::) [] (toDigits x)

-- ===================================
-- Ex. 2
-- ===================================

doubleSecond: List Int -> List Int
doubleSecond xs = 
    {-
    
    -}
    case xs of
        [] ->
            []
        -- A case where the first two items are extracted and ys is used for recursion
        y :: z :: ys ->
            y :: z * 2 :: doubleSecond ys
        -- Base recursive case
        y :: ys ->
            y :: doubleSecond ys
            
       
-- ===================================
-- Ex. 3
-- ===================================

sumDigits: List Int -> Int
sumDigits xs = 
    {- 
        Using a lambda function we split double-digit numbers 
        and sum all of the digits of the list.
        We use interger division to get the tens digit and
        modulo to get the remaining digit.
    -}
    List.foldr (\x acc -> (x // 10) + (modBy 10 x) + acc) 0 xs


-- ===================================
-- Ex. 4
-- ===================================

isValid: String -> Bool
isValid x = 
    modBy 10 (sumDigits (doubleSecond (toDigitsRev x))) == 0


-- ===================================
-- Ex. 5
-- ===================================
    
numValid: List String -> Int
numValid xs = 
    -- haskell: sum . map (\_ -> 1) $ filter isValid xs
    List.length (List.filter isValid xs)

countValidCards: List Int -> Int
countValidCards testCards =
    numValid (List.map String.fromInt testCards)

creditcards: List Int
creditcards = [ 4716347184862961,
                4532899082537349,
                4485429517622493,
                4320635998241421,
                4929778869082405,
                5256283618614517,
                5507514403575522,
                5191806267524120,
                5396452857080331,
                5567798501168013,
                6011798764103720,
                6011970953092861,
                6011486447384806,
                6011337752144550,
                6011442159205994,
                4916188093226163,
                4916699537435624,
                4024607115319476,
                4556945538735693,
                4532818294886666,
                5349308918130507,
                5156469512589415,
                5210896944802939,
                5442782486960998,
                5385907818416901,
                6011920409800508,
                6011978316213975,
                6011221666280064,
                6011285399268094,
                6011111757787451,
                4024007106747875,
                4916148692391990,
                4916918116659358,
                4024007109091313,
                4716815014741522,
                5370975221279675,
                5586822747605880,
                5446122675080587,
                5361718970369004,
                5543878863367027,
                6011996932510178,
                6011475323876084,
                6011358905586117,
                6011672107152563,
                6011660634944997,
                4532917110736356,
                4485548499291791,
                4532098581822262,
                4018626753711468,
                4454290525773941,
                5593710059099297,
                5275213041261476,
                5244162726358685,
                5583726743957726,
                5108718020905086,
                6011887079002610,
                6011119104045333,
                6011296087222376,
                6011183539053619,
                6011067418196187,
                4532462702719400,
                4420029044272063,
                4716494048062261,
                4916853817750471,
                4327554795485824,
                5138477489321723,
                5452898762612993,
                5246310677063212,
                5211257116158320,
                5230793016257272,
                6011265295282522,
                6011034443437754,
                6011582769987164,
                6011821695998586,
                6011420220198992,
                4716625186530516,
                4485290399115271,
                4556449305907296,
                4532036228186543,
                4916950537496300,
                5188481717181072,
                5535021441100707,
                5331217916806887,
                5212754109160056,
                5580039541241472,
                6011450326200252,
                6011141461689343,
                6011886911067144,
                6011835735645726,
                6011063209139742,
                379517444387209,
                377250784667541,
                347171902952673,
                379852678889749,
                345449316207827,
                349968440887576,
                347727987370269,
                370147776002793,
                374465794689268,
                340860752032008,
                349569393937707,
                379610201376008,
                346590844560212,
                376638943222680,
                378753384029375,
                348159548355291,
                345714137642682,
                347556554119626,
                370919740116903,
                375059255910682,
                373129538038460,
                346734548488728,
                370697814213115,
                377968192654740,
                379127496780069,
                375213257576161,
                379055805946370,
                345835454524671,
                377851536227201,
                345763240913232
              ]

-- collecting results for printing:

testToDigitsUnevenNumberedString: Bool
testToDigitsUnevenNumberedString = (toDigits "379055805946370" == [3,7,9,0,5,5,8,0,5,9,4,6,3,7,0])

testToDigitEvenNumberedString: Bool
testToDigitEvenNumberedString  = (toDigits "4716347184862961" == [4,7,1,6,3,4,7,1,8,4,8,6,2,9,6,1])


testToDigitRevUneven: Bool
testToDigitRevUneven = (toDigitsRev "345763240913232" == [2,3,2,3,1,9,0,4,2,3,6,7,5,4,3])

testToDigitRevEven: Bool
testToDigitRevEven = (toDigitsRev "4716347184862961" == [1,6,9,2,6,8,4,8,1,7,4,3,6,1,7,4])


testDoubleSecondEven: Bool
testDoubleSecondEven = (doubleSecond [4,7,1,6,3,4,7,1,8,4,8,6,2,9,6,1] == [4,14,1,12,3,8,7,2,8,8,8,12,2,18,6,2])


testDoubleSecondUneven: Bool
testDoubleSecondUneven = (doubleSecond [3,4,5,7,6,3,2,4,0,9,1,3,2,3,2] == [3,8,5,14,6,6,2,8,0,18,1,6,2,6,2])


testSumDigitsUneven: Bool
testSumDigitsUneven = (sumDigits [3,8,5,14,6,6,2,8,0,18,1,6,2,6,2] == 69)

testSumDigitsEven: Bool
testSumDigitsEven = (sumDigits [4,14,1,12,3,8,7,2,8,8,8,12,2,18,6,2] == 79)


-- Test a valid card
testIsValid: Bool
testIsValid = (isValid "4716347184862961" == True )

-- Test an invald card
testIsNOTvalid: Bool
testIsNOTvalid = (isValid "4532899082537349" == False)


testNumValid: Bool
testNumValid = (numValid ["4532899082537349", "4485429517622493", "4320635998241421", "4929778869082405", "5256283618614517" ] == 2)


testNumValidNoValidCards: Bool
testNumValidNoValidCards = (numValid ["5567798501168013", "4532818294886666"] == 0)


testExampleCreditCards: Bool
testExampleCreditCards = (countValidCards creditcards == 94)

allTests: List Bool
allTests = [testToDigitsUnevenNumberedString, testToDigitEvenNumberedString, testToDigitRevUneven, testToDigitRevEven, testDoubleSecondEven, testDoubleSecondUneven, testSumDigitsUneven, testSumDigitsEven, testIsValid, testIsNOTvalid, testNumValid, testNumValidNoValidCards, testExampleCreditCards]


my_results =
    [
        "calculations:",
        "-- end --"
    ] 
    
-- create main method (Boiler-plate)

to_div my_value = 
    Html.div [] [ my_value |> Html.text ]

main = Html.div 
        []
        (List.map to_div my_results)
    
