module Modelling2 exposing (..)
import Modelling1 exposing (..)
import HighOrder exposing (..)

{-
    Modelling Math Functions (part 2)
    Dimitar Byalkov and Kaloyan Stoykov
-}

f: Function
f = Plus (Mult (Plus (Const 3) X) (Minus X (Poly X 5))) (Const 2)

derivative: Function -> Function
derivative func =
    case func of
    -- constant
        Const _ ->
            Const 0
    -- X
        X ->
            Const 1
    -- sum
        Plus left right ->
            Plus (derivative left) (derivative right)
    -- difference
        Minus left right ->
            Minus (derivative left) (derivative right)
    -- product
        Mult left right ->
            Plus (Mult (derivative left) right) (Mult left (derivative right))
    -- quotient
        Div left right ->
            Div (Minus (Mult (derivative left) right) (Mult left (derivative right))) 
                (Poly right 2)
    -- power
        Poly X right ->
            Mult (Const right) (Poly X (right - 1))
        Poly left right ->
            Poly (derivative left) right

simplify: Function -> Function
simplify func = 
    -- tries to simplify until no further simplification is possible
    repeatUntil (\x -> x == simplifyHelp x) simplifyHelp func

simplifyHelp: Function -> Function
simplifyHelp func =
    case func of
        -- arithmetics
        Mult (Const left) (Const right) ->
            Const (left * right)
        Div (Const left) (Const right) ->
            Const (left // right)
        Plus (Const left) (Const right) ->
            Const (left + right)
        Minus (Const left) (Const right) ->
            Const (left - right)
        -- nested addition and subtraction
        Poly left 1 ->
            left
        Poly _ 0 ->
            (Const 1)
        Poly (Const left) right ->
            Const (left ^ right)
        Poly left right ->
            Poly left right
        Mult (Const 1) right ->
            right
        Mult left (Const 1) ->
            left
        Mult _ (Const 0) ->
            (Const 0)
        Div (Const 0) _ ->
            (Const 0)
        Div left (Const 1) ->
            left
        Plus left (Const 0) ->
            left
        Plus (Const 0) right ->
            right
        Minus left (Const 0) ->
            left
        -- recursive simplification
        Mult left right ->
            Mult (simplify left) (simplify right)
        Div left right ->
            Div (simplify left) (simplify right)
        Plus left right ->
            Plus (simplify left) (simplify right)
        Minus left right ->
            Minus (simplify left) (simplify right)
        _ ->
            func

-- Tests
testPrintFunc: Bool
testPrintFunc = print f == "(((3+x)*(x-(x^5)))+2)"

testPrintDerivFunc: Bool
testPrintDerivFunc = print (derivative f) == 
    "((((0+1)*(x-(x^5)))+((3+x)*(1-(5*(x^4)))))+0)"

testPrintSimplifyDerivFunc: Bool
testPrintSimplifyDerivFunc = print (simplify (derivative f)) == 
    "((x-(x^5))+((3+x)*(1-(5*(x^4)))))"

testDerivX: Bool
testDerivX = derivative X == Const 1

testDerivConst: Bool
testDerivConst = derivative (Const 5) == Const 0

testDerivPlus: Bool
testDerivPlus = derivative (Plus X (Const 9)) == Plus (Const 1) (Const 0)

testDerivMult: Bool
testDerivMult = derivative (Mult (Const 4) X) == 
    Plus (Mult (Const 0) X) (Mult (Const 4) (Const 1))

testDerivPolyX: Bool
testDerivPolyX = derivative (Poly X 3) == Mult (Const 3) (Poly X 2)

testDerivPoly: Bool
testDerivPoly = derivative (Poly (Const 4) 3) == (Poly (Const 0) 3)

testSimplePoly: Bool
testSimplePoly = simplify (Poly (Const 0) 3) == Const 0

testSimpleRecursivePlus: Bool
testSimpleRecursivePlus = simplify (Plus ((Mult (Const 1) X)) (Minus X (Const 0))) == (Plus X X)

allModelling2Tests: List Bool
allModelling2Tests = [testPrintFunc, testPrintDerivFunc, testPrintSimplifyDerivFunc, testDerivX, testDerivConst, testDerivPlus, testDerivMult, testDerivPolyX, testDerivPoly, testSimplePoly, testSimpleRecursivePlus]
