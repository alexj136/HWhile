module REPL where

import Lexer
import SourceParser
import PureSyntax
import PureInterpreter (evalExpr, Store)
import InterSyntax

import Data.List (isPrefixOf)
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State

type Breakpoint = (FilePath, Int)

data REPLCommand
    = EvalExp Expression
    | ExecComm InCommand
    | LoadProg FilePath
    | RunProg
    | StepProg
    | SetBreakpoint Breakpoint
    | DelBreakpoint Breakpoint
    | PrintHelp
    | Quit
    deriving (Show, Eq, Ord)

type REPLState = (Store, Maybe InCommand, S.Set Breakpoint)

emptyREPLState :: REPLState
emptyREPLState = (M.empty, Nothing, S.empty)

replLoop :: REPLState -> IO ()
replLoop replState = do
    (toContinue, replState') <- (flip runStateT) replState $ do 
        line <- replRead
        case replParse line of
            Nothing -> undefined
            Just replComm -> replEval replComm
    if toContinue then
        replLoop replState'
    else
        return ()

replRead :: StateT REPLState IO String
replRead = lift getLine

replParse :: String -> Maybe REPLCommand
replParse str
    | "load " `isPrefixOf` str = undefined
    | "help"  ==           str = Just PrintHelp
    | "quit"  ==           str = Just Quit
    | otherwise                = undefined

replEval :: REPLCommand -> StateT REPLState IO Bool
replEval replComm = case replComm of
    EvalExp exp -> do
        store <- getStore
        replPutStrLn . show $ evalExpr store exp
        return True
    ExecComm comm -> do
        store <- getStore
        let store' = evalInCommand store comm
        putStore store'
        return True
    LoadProg filepath -> undefined
    RunProg           -> undefined
    StepProg          -> undefined
    SetBreakpoint bp  -> undefined
    DelBreakpoint bp  -> undefined
    PrintHelp -> do
        replPutStrLn helpString
        return True
    Quit -> return False

evalInCommand :: Store -> InCommand -> Store
evalInCommand store comm = case comm of
    InAssign _ n e -> M.insert n (evalExpr store e) store
    InWhile  _ gd blk -> case evalExpr store gd of
        ENil -> store
        _    -> evalInCommand (evalInBlock store blk) comm
    InIfElse _ gd tb fb -> case evalExpr store gd of
        ENil -> evalInBlock store fb
        _    -> evalInBlock store tb
    InSwitch _ gd [] def -> evalInBlock store def
    InSwitch i gd ((e, blk) : cases) def ->
        if evalExpr store gd == evalExpr store e then
            evalInBlock store blk
        else
            evalInCommand store (InSwitch i gd cases def)

evalInBlock :: Store -> InBlock -> Store
evalInBlock = foldl evalInCommand

getStore :: StateT REPLState IO Store
getStore = do (store, _, _) <- get ; return store

putStore :: Store -> StateT REPLState IO ()
putStore store = do (_, mc, bps) <- get ; put (store, mc, bps) ; return ()

getFromStore :: Name -> StateT REPLState IO (Maybe ETree)
getFromStore name = do store <- getStore ; return $ M.lookup name store

putInStore :: Name -> ETree -> StateT REPLState IO ()
putInStore n t = do store <- getStore ; putStore $ M.insert n t store

replPutStrLn :: String -> StateT REPLState IO ()
replPutStrLn = lift . putStrLn

helpString :: String
helpString = "RTFM"
