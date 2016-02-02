module PureInterpreter where

{--
Interpreter functions for while programs. These functions are essentially
Haskell encodings of the semantic rules for the while language given in Neil
Jones' book on pages 40 & 41.
--}

import PureSyntax
import qualified Data.Map as M

-- While stores are maps from variable names to expressions, i.e. the values of
-- variables
type Store = M.Map Name Expression

-- To evaluate a program, we evaluate the program's command with the initial
-- store that contains the read variable with the value of the input, and
-- output (or 'write') the value of the write-variable in the resulting store.
evalProg :: Expression -> Program -> Expression
evalProg input (Program rd comm wrt) = evalExprNorm str' wrt
    where str' = evalComm (M.singleton rd input) comm

-- Commands update the contents of the store:
--   Assignments update the assignee variable with the assigned value.
--   While loops evaluate a guard expression with an initial store. If the
--     resulting expression is nil, we do nothing. Otherwise, we update the
--     store by executing the while loop's command, and repeat the process with
--     the new store.
--   Sequential Composition is evaluated by first evaluating command one with an
--     initial store, and then evaluating command two with the resulting store.
evalComm :: Store -> Command -> Store
evalComm str (Compos a b) = evalComm str' b
    where str' = evalComm str a
evalComm str (Assign v x) = M.insert v (evalExprNorm str x) str
evalComm str (While  x c) = case evalExprNorm str x of
    Nil -> str
    _   -> evalComm str' (While x c)
        where str' = evalComm str c

-- Expression evaluation is straightforward - see page 40 of Neil Jones' book
-- for more detail. This function performs a single reduction step.
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

-- Repeatedly evaluate an expression until it reaches a 'normal form' i.e. until
-- it cannot be evaluated any further.
evalExprNorm :: Store -> Expression -> Expression
evalExprNorm str exp | nextReduce == exp = exp
                     | otherwise         = evalExprNorm str nextReduce
    where nextReduce = evalExpr str exp
