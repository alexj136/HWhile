module PureInterpreter where

{--
Interpreter functions for while programs. These functions are essentially
Haskell encodings of the semantic rules for the while language given in Neil
Jones' book on pages 40 & 41.
--}

import PureSyntax
import qualified Data.Map as M

-- While stores are maps from variable names to trees, i.e. the values of
-- variables
type Store = M.Map Name ETree

-- To evaluate a program, we evaluate the program's command with the initial
-- store that contains the read variable with the value of the input, and
-- output (or 'write') the value of the write-variable in the resulting store.
evalProg :: ETree -> Program -> ETree
evalProg input (Program rd blk wrt) = M.findWithDefault ENil wrt store
    where store = evalBlock (M.singleton rd input) blk

-- To evaluate an empty block, just return the input store. To evaluate a non-
-- empty block, evaluate the first element with the input store, and then
-- evaluate the rest of the block with the resulting store.
evalBlock :: Store -> Block -> Store
evalBlock store []     = store
evalBlock store (c:cs) = let store' = evalComm store c in evalBlock store' cs

-- Commands update the contents of the store:
--   Assignments update the assignee variable with the assigned value.
--   While loops evaluate a guard expression with an initial store. If the
--     resulting expression is nil, we do nothing. Otherwise, we update the
--     store by executing the while loop's command, and repeat the process with
--     the new store.
--   Conditionals are evaluated by first evaluating the condition. If the
--     condition is false (nil), the 'else-block' is evaluated. Otherwise the
--     'then-block' is evaluated.
evalComm :: Store -> Command -> Store
evalComm store (Assign v x)   = M.insert v (evalExpr store x) store
evalComm store (While  x b)   = case evalExpr store x of
    ENil -> store
    _    -> let store' = evalBlock store b in evalComm store' (While x b)
evalComm store (IfElse e a b) = case evalExpr store e of
    ENil -> evalBlock store b
    _    -> evalBlock store a

-- Expression evaluation is straightforward - see page 40 of Neil Jones' book
-- for more detail. This function performs a single reduction step.
evalExpr :: Store -> Expression -> ETree
evalExpr store expr = let eval = evalExpr store in case expr of
    Var  n   -> M.findWithDefault ENil n store
    Hd   e   -> case eval e of { ENil -> ENil; ECons h _ -> h}
    Tl   e   -> case eval e of { ENil -> ENil; ECons _ t -> t}
    IsEq a b -> if eval a == eval b then ECons ENil ENil else ENil
    Cons h t -> ECons (eval h) (eval t)
    Lit  t   -> t
