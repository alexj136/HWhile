module REPL where

import Lexer
import SourceParser
import PureSyntax
import PureInterpreter (evalExpr, Store)
import InterSyntax

import Data.List (isPrefixOf)
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State.Strict
import qualified System.Console.Haskeline as HL
import qualified System.Console.Repline   as RL

type REPL a = RL.HaskelineT IO a

runREPL :: IO ()
runREPL = RL.evalRepl "HWhile> " tryParseRunPrint options
    (RL.Word0 completer) initialise

tryParseRunPrint :: String -> REPL ()
tryParseRunPrint = error "not yet implemented"

initialise :: REPL ()
initialise = liftIO $ putStrLn welcomeString

options :: [(String, [String] -> REPL ())]
options =
    [ ("help"    , printHelp)
    , ("quit"    , doQuit   )
    , ("load"    , undefined)
    , ("run"     , undefined)
    , ("step"    , undefined)
    , ("break"   , undefined)
    , ("delbreak", undefined)
    ]

printHelp :: [String] -> REPL ()
printHelp _ = liftIO $ putStrLn helpString

doQuit :: [String] -> REPL ()
doQuit _ = error "not yet implemented"

completer :: Monad m => RL.WordCompleter m
completer str = do
    let completionWords = [ "while" , "switch" , "nil" ]
    return $ filter (str `isPrefixOf`) completionWords

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

replPutStrLn :: String -> StateT REPLState IO ()
replPutStrLn = lift . putStrLn

helpString :: String
helpString = "RTFM"

welcomeString :: String
welcomeString = "Welcome to HWhile interactive mode. Type ':help' for more " ++
    "information."
