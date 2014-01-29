module Interpreter where

import Syntax
import qualified Data.Map as M

-- Interpreter for while programs

type Context = M.Map String Expression

evalProg :: Expression -> Program -> Expression
evalProg input (Program r cs w) = evalExpr ctx' w
    where ctx' = evalComms (M.singleton r input) cs

evalComms :: Context -> [Command] -> Context
evalComms ctx []     = ctx
evalComms ctx (c:cs) = evalComms (evalComm ctx c) cs

evalComm :: Context -> Command -> Context
evalComm ctx (Assign v x) = M.insert v x ctx
evalComm ctx (While x cs) = case evalExpr ctx x of
    Nil -> ctx
    _   -> evalComm ctx' (While x cs)
        where ctx' = evalComms ctx cs

evalExpr :: Context -> Expression -> Expression
evalExpr ctx expr = case expr of
    Var s         -> case M.lookup s ctx of
        Nothing -> error $ "Variable " ++ s ++ " used before initialisation"
        Just x  -> evalExpr ctx x
    Hd (Cons a b) -> evalExpr ctx a
    Hd other      -> error $ "Cannot take head of expression " ++ show other
    Tl (Cons a b) -> evalExpr ctx b
    Tl other      -> error $ "Cannot take tail of expression " ++ show other
    IsEq a b | evalExpr ctx a == evalExpr ctx b -> Cons Nil Nil
             | otherwise                        -> Nil
    Cons a b      -> Cons (evalExpr ctx a) (evalExpr ctx b)
    Nil           -> Nil
