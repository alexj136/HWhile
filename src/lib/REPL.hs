module REPL where

import Lexer
import SourceParser
import PureSyntax
import PureInterpreter (evalExpr, Store)
import DesugarSI (desugarComm)
import InterSyntax

import Data.List (isPrefixOf, intersperse)
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State.Strict
import Control.Monad.Except
import qualified System.Console.Haskeline as HL
import qualified System.Console.Repline   as RL

type REPL a = RL.HaskelineT (StateT REPLState IO) a

runREPL :: StateT REPLState IO ()
runREPL = RL.evalRepl "HWhile> " execute options (RL.Word0 completer) initialise

execute :: String -> REPL ()
execute str = do
    let tks = scan str "+IMPL+"
    expTry <- lift $ lift $ runExceptT $ parseExpr tks
    case expTry of
        Left err -> do
            commTry <- lift $ lift $ runExceptT $
                do suComm <- parseComm tks ; desugarComm "." [] suComm
            case commTry of
                Left err   -> replPutStrLn err
                Right comm -> do
                    store <- lift getStore
                    lift $ putStore $ evalInBlock store comm
        Right exp -> do
            store <- lift getStore
            replPutStrLn $ show $ evalExpr store exp

initialise :: REPL ()
initialise = liftIO $ putStrLn welcomeString

options :: [(String, [String] -> REPL ())]
options =
    [ ("help"    , help     )
    , ("load"    , undefined)
    , ("run"     , undefined)
    , ("step"    , undefined)
    , ("break"   , undefined)
    , ("delbreak", undefined)
    ]

help :: [String] -> REPL ()
help _ = liftIO $ putStrLn helpString

completer :: Monad m => RL.WordCompleter m
completer str = do
    let completionWords = [ "while" , "switch" , "nil" ]
    return $ filter (str `isPrefixOf`) completionWords

replPutStrLn :: String -> REPL ()
replPutStrLn = lift . lift . putStrLn

type Breakpoint = (FilePath, Int)

type REPLState = (Store, Maybe InCommand, S.Set Breakpoint)

emptyREPLState :: REPLState
emptyREPLState = (M.empty, Nothing, S.empty)

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

helpString :: String
helpString = concat $ (intersperse "\n") $
    [ "HWhile interactive mode. Possible options:"
    , "    <EXPR>    - evaluate a while expression"
    , "    <COMM>    - execute a while command"
    , "    :help     - print this message"
    , "    (Ctrl+D)  - quit"
    ]

welcomeString :: String
welcomeString = "Welcome to HWhile interactive mode. Type ':help' for more " ++
    "information."
