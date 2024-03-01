module Caesar1 exposing (..)
{-
    Caesar cipher function
    Dimitar Byalkov and Kaloyan Stoykov
-}

encode: Int -> Char -> Char
encode offset character =
    -- 65 A 90 Z
    if Char.toCode(character) > 64 && Char.toCode(character) < 91 then
        Char.fromCode(65 + modBy 26 (Char.toCode(character) - 65 + offset - 26))
    -- 97 a 122 z
    else if Char.toCode(character) > 96 && Char.toCode(character) < 123 then
        Char.fromCode(97 + modBy 26 (Char.toCode(character) - 97 + offset - 26))
    else Char.fromCode(32)

decode: Int -> Char -> Char
decode offset character = 
    encode (0 - offset) character

-- Tests

testEncodeUpperSmallOffsetA: Bool
testEncodeUpperSmallOffsetA = (encode 5 'A' == 'F')

testEncodeUpperBigOffsetF: Bool
testEncodeUpperBigOffsetF = (encode 348 'F' == 'P')

testEncodeLowerSmallOffsetQ: Bool
testEncodeLowerSmallOffsetQ = (encode 9 'q' == 'z')

testEncodeLowerBigOffsetG: Bool
testEncodeLowerBigOffsetG = (encode 215 'g' == 'n')

testDecodeUpperSmallOffsetA: Bool
testDecodeUpperSmallOffsetA = (decode 5 'F' == 'A')

testDecodeUpperBigOffsetF: Bool
testDecodeUpperBigOffsetF = (decode 348 'P' == 'F')

testDecodeLowerSmallOffsetQ: Bool
testDecodeLowerSmallOffsetQ = (decode 9 'z' == 'q')

testDecodeLowerBigOffsetG: Bool
testDecodeLowerBigOffsetG = (decode 215 'n' == 'g')

allTests: List Bool
allTests = [testEncodeUpperSmallOffsetA, testEncodeUpperBigOffsetF, testEncodeLowerSmallOffsetQ, testEncodeLowerBigOffsetG, testDecodeUpperSmallOffsetA, testDecodeUpperBigOffsetF, testDecodeLowerSmallOffsetQ, testDecodeLowerBigOffsetG]