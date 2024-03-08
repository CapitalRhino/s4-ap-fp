module Caesar2 exposing (..)

{-
    Caesar cipher (Part 2)
    Dimitar Byalkov and Kaloyan Stoykov
-}

encode: Int -> Char -> Char
encode offset character =
    -- Char.isUpper replaces the check for the ASCII code range of uppercase letters
    if Char.isUpper character then
        shiftChar (Char.toCode 'A') offset character
    -- Char.isLower replaces the check for the ASCII code range of lowercase letters
    else if Char.isLower character then
        shiftChar (Char.toCode 'a') offset character
    else ' '

-- shiftChar takes the common arithmetic actions from the if-statement in 'encode'
shiftChar: Int -> Int -> Char -> Char
shiftChar baseCode offset character =
    Char.fromCode(baseCode + modBy 26 ((Char.toCode character) - baseCode + offset - 26))

decode: Int -> Char -> Char
decode offset character = 
    encode (0 - offset) character


{- Return a list with a filter function that keeps only uppercase and lowercase ascii chars. -}
normalize: String -> String
normalize input =
    String.fromList (List.filter Char.isAlpha (String.toList input))


encrypt: Int -> String -> String
encrypt offset input = 
    case String.uncons input of
        -- Check for empty string
        Nothing ->
            ""
        -- When string is split into form of "a", "bc" for example, encode the head and call recursively with the same offset -> encoded char :: encrypt offset tail
        Just (head, tail) ->
            String.cons (encode offset head) (encrypt offset tail)

{- Mirrored version of encrypt with decode and decrypt used instead -}
decrypt: Int -> String -> String
decrypt offset input = 
    case String.uncons input of
        Nothing ->
            ""
        Just (head, tail) ->
            String.cons (decode offset head) (decrypt offset tail)




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


testShiftChar: Bool
testShiftChar  = (shiftChar 97 3 'a' == 'd')


testNormalize: Bool
testNormalize = (normalize "Hello, Fontys!" == "HelloFontys")


testNormalizeB: Bool
testNormalizeB = (normalize "Hello#@#!#@" == "Hello")


testEncrypt: Bool
testEncrypt = (encrypt 7 "HelloFontys" == "OlssvMvuafz")


testEncryptB: Bool
testEncryptB = (encrypt 6 "RandomText" == "XgtjusZkdz")


testDecrypt: Bool
testDecrypt = (decrypt 6 "Hsdsakw" == "Bmxmueq")


testDecryptB: Bool
testDecryptB = (decrypt 9 "HSAhsh" == "YJRyjy")


allCaesar2Tests = [testEncodeUpperSmallOffsetA, testEncodeUpperBigOffsetF, testEncodeLowerSmallOffsetQ, testEncodeLowerBigOffsetG, testDecodeUpperSmallOffsetA, testDecodeUpperBigOffsetF, testDecodeLowerSmallOffsetQ, testDecodeLowerBigOffsetG, testShiftChar, testNormalize, testNormalizeB, testEncrypt, testEncryptB, testDecrypt, testDecryptB]




