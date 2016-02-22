module LoggingInterpreter where

{-
Interpreter that prints a log of each assignment to the command line. Threads a
function used to print PureSyntax.ETrees.
-}

import PureSyntax
import qualified PureInterpreter as I
import qualified Data.Map as M

evalProg :: (ETree -> String) -> ETree -> Program -> IO ETree
evalProg showFunction input (Program rd comm wrt) = do
    store <- evalComm showFunction (M.singleton rd input) comm
    return $ M.findWithDefault ENil wrt store

evalComm :: (ETree -> String) -> I.Store -> Command -> IO I.Store
evalComm showFunction store comm = case comm of
    Compos a b   -> do
        store' <- evalComm showFunction store a
        evalComm showFunction store' b
    Assign v x   -> let evalX = I.evalExpr store x in do
        putStrLn $ displayAssignment showFunction v evalX
        return $ M.insert v evalX store
    While  x c   -> case I.evalExpr store x of
        ENil -> return store
        _    -> do
            store' <- evalComm showFunction store c
            evalComm showFunction store' (While x c)
    IfElse e a b -> case I.evalExpr store e of
        ENil -> evalComm showFunction store b
        _    -> evalComm showFunction store a

displayAssignment :: (ETree -> String) -> Name -> ETree -> String
displayAssignment showFunction (Name (fp, n)) tree =
    concat $ ["(", fp, ") ", n, " := ", showFunction tree]
