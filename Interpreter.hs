module Interpreter where

import Syntax
import qualified Data.Map as M

-- Interpreter for while programs

type Store = M.Map String Expression

evalProg :: Expression -> Program -> Expression
evalProg input (Program rd comm wrt) = evalExprNorm σ' wrt
    where σ' = evalComm (M.singleton rd input) comm

evalComm :: Store -> Command -> Store
evalComm σ (Compos a b) = evalComm σ' b
    where σ' = evalComm σ a
evalComm σ (Assign v x) = M.insert v (evalExprNorm σ x) σ
evalComm σ (While  x c) = case evalExprNorm σ x of
    Nil -> σ
    _   -> evalComm σ' (While x c)
        where σ' = evalComm σ c

evalExpr :: Store -> Expression -> Expression
evalExpr σ expr = case expr of
    Var s         -> case M.lookup s σ of { Nothing -> Nil ; Just x -> x }
    Hd (Cons a b) -> a
    Hd Nil        -> Nil
    Hd other      -> Hd (evalExpr σ other)
    Tl (Cons a b) -> b
    Tl Nil        -> Nil
    Tl other      -> Tl (evalExpr σ other)
    IsEq a b | evalExprNorm σ a == evalExprNorm σ b -> Cons Nil Nil
             | otherwise                            -> Nil
    Cons a b      -> Cons (evalExpr σ a) (evalExpr σ b)
    Nil           -> Nil

evalExprNorm :: Store -> Expression -> Expression
evalExprNorm σ exp | nextReduce == exp = exp
                   | otherwise       = evalExprNorm σ nextReduce
    where nextReduce = evalExpr σ exp
