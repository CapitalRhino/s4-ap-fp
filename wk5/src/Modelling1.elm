module Modelling1 exposing (..)

{-
    Modelling Math Functions (part 1)
    Dimitar Byalkov and Kaloyan Stoykov
-}

type Function
  = Poly  Function Int
  | Mult  Function Function
  | Div   Function Function
  | Plus  Function Function
  | Minus Function Function 
  | Const Int
  | X

print: Function -> String
print f =
    case f of
        X ->
            "x"
        Const val ->
            String.fromInt val
        Poly func val ->
            "(" ++ print func ++ " ^ " ++ String.fromInt val ++ ")"
        Mult func1 func2 ->
            "(" ++ print func1 ++ " * " ++ print func2 ++ ")"
        Div func1 func2 ->
            "(" ++ print func1 ++ " / " ++ print func2 ++ ")"
        Plus func1 func2 ->
            "(" ++ print func1 ++ " + " ++ print func2 ++ ")"
        Minus func1 func2 ->
            "(" ++ print func1 ++ " - " ++ print func2 ++ ")"
        
eval: Float -> Function -> Float
eval num f =
    case f of
        X ->
            num
        Const val ->
            toFloat val
        Poly func val ->
            eval num func ^ (toFloat val)
        Mult func1 func2 ->
            eval num func1 * eval num func2
        Div func1 func2 ->
            eval num func1 / eval num func2
        Plus func1 func2 ->
            eval num func1 + eval num func2
        Minus func1 func2 ->
            eval num func1 - eval num func2 

-- Graph line drawing function
graphLine: Function -> Int -> Int -> Int -> String
graphLine func x ymin ymax =
    let
        -- value of f(x)
        funcValue = round (eval (toFloat x) func)
        -- number of points on the line
        ySpan = abs ymin + abs ymax
    in
        -- Check if the value of f(x) is in the specified range
        if (funcValue > ymin) && (funcValue < ymax) then
            stringFill (funcValue - ymin) "*" ++ stringFill (ySpan - (funcValue - ymin)) "-"
        else if (funcValue > ymin) then
            stringFill ySpan "*"
        else
            stringFill ySpan "-"

-- Recursive function to create a string of specified characters with a given length
stringFill: Int -> String -> String
stringFill n sym =
    if (n <= 0) then
        ""
    else
        sym ++ stringFill (n - 1) sym

-- xmin < xmax; ymin < ymax
graph: Function -> Int -> Int -> Int -> Int -> String
graph func xmin xmax ymin ymax =
    -- Check if range is not possible
    if xmin > xmax || ymin > ymax then
      "Error: Range is negative"
    else if xmin == xmax then
        graphLine func xmin ymin ymax
    else
        graphLine func xmin ymin ymax ++ "\n" ++ graph func (xmin + 1) xmax ymin ymax

testPrint: Bool
testPrint = print (Plus (Mult (Plus (Const 3) X) (Minus X (Poly X 5))) (Const 2)) == "(((3 + x) * (x - (x ^ 5))) + 2)"

testEval: Bool
testEval = eval 2 (Plus (Mult (Plus (Const 3) X) (Minus X (Poly X 5))) (Const 2)) == -148

testGraph1: Bool
testGraph1 = graph (Plus (Minus (Poly (Minus (Div X (Const 5)) (Const 1)) 4) (Poly (Plus (Div X (Const -2)) (Const 2)) 2)) (Const 6)) -10 20 -10 10 == 
    "********************\n********************\n********************\n*******************-\n**************------\n************--------\n**********----------\n**********----------\n***********---------\n************--------\n*************-------\n**************------\n***************-----\n****************----\n****************----\n****************----\n***************-----\n**************------\n************--------\n**********----------\n********------------\n******--------------\n****----------------\n**------------------\n*-------------------\n**------------------\n***-----------------\n*******-------------\n*************-------\n********************\n********************"

-- for reference 3x2-graph.png
testGraph2: Bool
testGraph2 = graph (Mult (Const 3) (Poly X 2)) -5 5 -1 10 == "***********\n***********\n***********\n***********\n****-------\n*----------\n****-------\n***********\n***********\n***********\n***********"

testEmptyParams: Bool
testEmptyParams = graph (Mult (Const 3) (Poly X 2)) 0 0 0 0 == ""

testSingleLine: Bool
testSingleLine = graph (Mult (Const 3) (Poly X 2)) 1 1 -1 10 == "****-------"


testInvalidArgs: Bool
testInvalidArgs = graph X 2 1 2 1 == "Error: Range is negative"

testAllGraphs : List Bool
testAllGraphs = [testGraph1, testGraph2, testEmptyParams, testSingleLine, testInvalidArgs] 