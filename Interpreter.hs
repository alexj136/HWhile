module Interpreter where

import Syntax
import qualified Data.Map as M

-- Interpreter for while programs

type Store = M.Map String Expression

evalProg :: Expression -> Program -> Expression
evalProg input (Program rd comm wrt) = evalExprNorm str' wrt
    where str' = evalComm (M.singleton rd input) comm

evalComm :: Store -> Command -> Store
evalComm str (Compos a b) = evalComm str' b
    where str' = evalComm str a
evalComm str (Assign v x) = M.insert v (evalExprNorm str x) str
evalComm str (While  x c) = case evalExprNorm str x of
    Nil -> str
    _   -> evalComm str' (While x c)
        where str' = evalComm str c

evalExpr :: Store -> Expression -> Expression
evalExpr str expr = case expr of
    Var s         -> case M.lookup s str of { Nothing -> Nil ; Just x -> x }
    Hd (Cons a b) -> a
    Hd Nil        -> Nil
    Hd other      -> Hd (evalExpr str other)
    Tl (Cons a b) -> b
    Tl Nil        -> Nil
    Tl other      -> Tl (evalExpr str other)
    IsEq a b | evalExprNorm str a == evalExprNorm str b -> Cons Nil Nil
             | otherwise                                -> Nil
    Cons a b      -> Cons (evalExpr str a) (evalExpr str b)
    Nil           -> Nil

evalExprNorm :: Store -> Expression -> Expression
evalExprNorm str exp | nextReduce == exp = exp
                     | otherwise         = evalExprNorm str nextReduce
    where nextReduce = evalExpr str exp
