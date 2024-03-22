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