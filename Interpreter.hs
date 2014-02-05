module Interpreter where

import Syntax
import qualified Data.Map as M

-- Interpreter for while programs

type Store = M.Map String Expression

evalProg :: Expression -> Program -> Expression
evalProg input (Program r cs w) = evalExprNorm σ' w
    where σ' = evalComms (M.singleton r input) cs

evalComms :: Store -> [Command] -> Store
evalComms σ []     = σ
evalComms σ (c:cs) = evalComms (evalComm σ c) cs

evalComm :: Store -> Command -> Store
evalComm σ (Assign v x) = M.insert v (evalExprNorm σ x) σ
evalComm σ (While x cs) = case evalExprNorm σ x of
    Nil -> σ
    _   -> evalComm σ' (While x cs)
        where σ' = evalComms σ cs

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
