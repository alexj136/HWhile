module LoggingInterpreter where

{-
Interpreter that prints a log of each assignment to the command line. Threads a
function used to print PureSyntax.ETrees.
-}

import PureSyntax
import qualified PureInterpreter as I
import qualified Data.Map as M

evalProg :: (ETree -> String) -> ETree -> Program -> IO ETree
evalProg showFunction input (Program _ rd blk wrt) = do
    store <- evalBlock showFunction (M.singleton rd input) blk
    return $ M.findWithDefault ENil wrt store

evalBlock :: (ETree -> String) -> I.Store -> Block -> IO I.Store
evalBlock showFunction store blk = case blk of
    []   -> return store
    c:cs -> do
        store' <- evalComm showFunction store c
        evalBlock showFunction store' cs

evalComm :: (ETree -> String) -> I.Store -> Command -> IO I.Store
evalComm showFunction store comm = case comm of
    Assign v x   -> let evalX = I.evalExpr store x in do
        putStrLn $ displayAssignment showFunction v evalX
        return $ M.insert v evalX store
    While  x b   -> case I.evalExpr store x of
        ENil -> return store
        _    -> do
            store' <- evalBlock showFunction store b
            evalComm showFunction store' (While x b)
    IfElse e a b -> case I.evalExpr store e of
        ENil -> evalBlock showFunction store b
        _    -> evalBlock showFunction store a

displayAssignment :: (ETree -> String) -> Name -> ETree -> String
displayAssignment showFunction (Name (fp, n)) tree =
    concat $ ["(", fp, ") ", n, " := ", showFunction tree]
