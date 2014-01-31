module Interpreter where

import Syntax
import qualified Data.Map as M

-- Interpreter for while programs

type Context = M.Map String Expression

evalProg :: Expression -> Program -> Expression
evalProg input (Program r cs w) = evalExprNorm ctx' w
    where ctx' = evalComms (M.singleton r input) cs

evalComms :: Context -> [Command] -> Context
evalComms ctx []     = ctx
evalComms ctx (c:cs) = evalComms (evalComm ctx c) cs

evalComm :: Context -> Command -> Context
evalComm ctx (Assign v x) = M.insert v (evalExprNorm ctx x) ctx
evalComm ctx (While x cs) = case evalExprNorm ctx x of
    Nil -> ctx
    _   -> evalComm ctx' (While x cs)
        where ctx' = evalComms ctx cs

evalExpr :: Context -> Expression -> Expression
evalExpr ctx expr = case expr of
    Var s         -> case M.lookup s ctx of
        Nothing -> error $ "Variable " ++ s ++ " used before initialisation"
        Just x  -> x
    Hd (Cons a b) -> a
    Hd Nil        -> error "Cannot take head of nil"
    Hd other      -> Hd (evalExpr ctx other)
    Tl (Cons a b) -> b
    Tl Nil        -> error "Cannot take tail of nil"
    Tl other      -> Tl (evalExpr ctx other)
    IsEq a b | evalExprNorm ctx a == evalExprNorm ctx b -> Cons Nil Nil
             | otherwise                                -> Nil
    Cons a b      -> Cons (evalExpr ctx a) (evalExpr ctx b)
    Nil           -> Nil

evalExprNorm :: Context -> Expression -> Expression
evalExprNorm ctx exp | nextReduce == exp = exp
                     | otherwise         = evalExprNorm ctx nextReduce
    where nextReduce = evalExpr ctx exp
