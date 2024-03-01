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
    -- Char.isLowe replaces the check for the ASCII code range of lowercase letters
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

normalize: String -> String
normalize input =
    String.fromList (List.filter Char.isAlpha (String.toList input))

encrypt: Int -> String -> String
encrypt offset input = 
    case String.uncons input of
        Nothing ->
            ""
        Just (head, tail) ->
            String.cons (encode offset head) (encrypt offset tail)

decrypt: Int -> String -> String
decrypt offset input = 
    case String.uncons input of
        Nothing ->
            ""
        Just (head, tail) ->
            String.cons (decode offset head) (decrypt offset tail)