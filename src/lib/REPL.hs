module REPL (REPL, runREPL, emptyREPLState) where

import qualified Lexer        as L
import qualified SourceParser as SP
import qualified DesugarSI    as DS
import PureSyntax
import InterSyntax
import SugarSyntax
import PureInterpreter (evalExpr, Store)

import Data.List (isPrefixOf, intersperse, intercalate)
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State.Strict
import Control.Monad.Except
import qualified System.Console.Haskeline as HL
import qualified System.Console.Repline   as RL

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

type REPL a = RL.HaskelineT (StateT REPLState IO) a

type Breakpoint = (FilePath, Int)

type REPLState =
    ( Store             -- The store for interactive execution
    , Maybe InProgram   -- A loaded program that can be executed
    , S.Set Breakpoint  -- Breakpoints that we pause at
    , ETree -> String   -- Tree printing function
    , FilePath          -- Current file search path
    )

emptyREPLState :: REPLState
emptyREPLState = (M.empty, Nothing, S.empty, show, ".")

--------------------------------------------------------------------------------
-- Repline bits and pieces
--------------------------------------------------------------------------------

runREPL :: StateT REPLState IO ()
runREPL = RL.evalRepl "HWhile> " execute options (RL.Word0 completer) initialise

execute :: String -> REPL ()
execute str = do
    printFn <- getPrintFn
    path <- getPath
    expTry <- parseExpr str
    case expTry of
        Left err -> do
            suCommTry <- parseComm str
            case suCommTry of
                Left err   -> replPutStrLn err
                Right suComm -> do
                    commTry <- desugarComm path suComm
                    case commTry of
                        Left err   -> replPutStrLn err
                        Right comm -> do
                            store <- getStore
                            putStore $ evalInBlock store comm
        Right exp -> do
            store <- getStore
            replPutStrLn $ printFn $ evalExpr store exp

initialise :: REPL ()
initialise = replPutStrLn welcomeString

options :: [(String, [String] -> REPL ())]
options =
    [ ("help"     , help     )
    , ("load"     , load     )
    , ("run"      , run      )
    , ("printmode", printmode)
    , ("cd"       , cd       )
    , ("step"     , undefined)
    , ("break"    , undefined)
    , ("delbreak" , undefined)
    ]

help :: [String] -> REPL ()
help _ = liftIO $ putStrLn helpString

load :: [String] -> REPL ()
load args
    | length args == 0 =
        replPutStrLn "Please provide a while program name."
    | length args  > 1 =
        replPutStrLn "Please provide a single while program name."
    | otherwise        = do
        path <- getPath
        eProg <- loadProg path (head args)
        case eProg of
            Left err   -> replPutStrLn err
            Right prog -> do
                replPutStrLn $ "Program '" ++ (head args) ++ "' loaded."
                putProg prog

run :: [String] -> REPL ()
run args
    | length args == 0 =
        replPutStrLn "Please supply an argument literal (e.g. <nil.nil>)."
    | otherwise        = do
        printFn <- getPrintFn
        mProg <- getProg
        case mProg of
            Nothing -> replPutStrLn
                "No program loaded. Please load one with ':load'."
            Just prog -> do
                argTry <- parseLVal (intercalate " " args)
                case argTry of
                    Left  err -> replPutStrLn err
                    Right arg -> replPutStrLn $ printFn $ evalInProgram arg prog

printmode :: [String] -> REPL ()
printmode args
    | length args /= 1 =
        replPutStrLn "Please supply a single print mode string."
    | otherwise        = case head args of
        "i"   -> setPrintFn $ \tree -> maybe "E" show $ parseInt tree
        "iv"  -> setPrintFn $ \tree -> maybe (show tree) show $ parseInt tree
        "l"   -> setPrintFn $ show . toHaskellList
        "li"  -> setPrintFn $ showIntListTree False
        "liv" -> setPrintFn $ showIntListTree True
        "L"   -> setPrintFn $ showNestedIntListTree
        "La"  -> setPrintFn $ showNestedAtomIntListTree
        _     -> replPutStrLn $ "Error - valid modes are i, iv, l, li, liv, " ++
            "L, and La. Quit interactive mode and then run 'hwhile -h' for " ++
            "more information."

cd :: [String] -> REPL ()
cd args
    | length args /= 1 = replPutStrLn "Please supply a single directory path."
    | otherwise        = putPath (head args)

completer :: Monad m => RL.WordCompleter m
completer str = do
    let completionWords = [
            "nil"    , "cons"      , "hd"      , "tl"    , "while"   ,
            "switch" , "case"      , "default" , "if"    , "else"    ,
            "true"   , "false"     , "@:="     , "@asgn" , "@doAsgn" ,
            "@while" , "@doWhile"  , "@if"     , "@doIf" , "@var"    ,
            "@quote" , "@hd"       , "@doHd"   , "@tl"   , "@doTl"   ,
            "@cons"  , "@doCons"   ]
            ++ map ((':' :) . fst) options
    return $ filter (str `isPrefixOf`) completionWords

replPutStrLn :: String -> REPL ()
replPutStrLn = lift . lift . putStrLn

--------------------------------------------------------------------------------
-- InProgram/InCommand/Inblock evaluators for interactivity
--------------------------------------------------------------------------------

evalInProgram :: ETree -> InProgram -> ETree
evalInProgram input (InProgram _ rd blk wrt) = M.findWithDefault ENil wrt store
    where store = evalInBlock (M.singleton rd input) blk

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

--------------------------------------------------------------------------------
-- REPLState monadic getters and setters
--------------------------------------------------------------------------------

getStore :: REPL Store
getStore = do (store, _, _, _, _) <- lift get ; return store

getPrintFn :: REPL (ETree -> String)
getPrintFn = do (_, _, _, pf, _) <- lift get ; return pf

setPrintFn :: (ETree -> String) -> REPL ()
setPrintFn pf = do
    (store, mc, bps, _, fp) <- lift get
    lift $ put (store, mc, bps, pf, fp)
    return ()

putStore :: Store -> REPL ()
putStore store = do
    (_, mc, bps, pf, fp) <- lift get
    lift $ put (store, mc, bps, pf, fp)
    return ()

getFromStore :: Name -> REPL (Maybe ETree)
getFromStore name = do store <- getStore ; return $ M.lookup name store

putInStore :: Name -> ETree -> REPL ()
putInStore n t = do store <- getStore ; putStore $ M.insert n t store

getProg :: REPL (Maybe InProgram)
getProg = do (_, mp, _, _, _) <- lift get ; return mp

putProg :: InProgram -> REPL ()
putProg prog = do
    (store, _, bps, pf, fp) <- lift get
    lift $ put (store, Just prog, bps, pf, fp)
    return ()

getPath :: REPL FilePath
getPath = do (_, _, _, _, fp) <- lift get ; return fp

putPath :: String -> REPL ()
putPath fp = do
    (store, mp, bps, pf, _) <- lift get
    lift $ put (store, mp, bps, pf, fp)
    return ()

--------------------------------------------------------------------------------
-- Parser helpers in the REPL monad
--------------------------------------------------------------------------------

parseExpr :: String -> REPL (Either String Expression)
parseExpr = lift . lift . runExceptT . SP.parseExpr . (flip L.scan "+IMPL+")

parseComm :: String -> REPL (Either String SuCommand)
parseComm = lift . lift . runExceptT . SP.parseComm . (flip L.scan "+IMPL+")

parseLVal :: String -> REPL (Either String ETree)
parseLVal = lift . lift . runExceptT . SP.parseLVal . (flip L.scan "+IMPL+")

desugarComm :: FilePath {- Macro search path -} -> SuCommand ->
    REPL (Either String InBlock)
desugarComm path = lift. lift . runExceptT . DS.desugarComm path []

loadProg ::
    FilePath -> -- Search path
    FilePath -> -- Program name
    REPL (Either String InProgram)
loadProg path progName = lift . lift . runExceptT $ DS.loadProg path progName []

--------------------------------------------------------------------------------
-- Some printable contents
--------------------------------------------------------------------------------

helpString :: String
helpString = concat $ (intersperse "\n") $
    [ "HWhile interactive mode. Possible options:"
    , "    <EXPR>       - Evaluate a while expression."
    , "    <COMM>       - Execute a while command."
    , "    :help        - Print this message."
    , "    :load p      - Load a while program 'p' (i.e. from the file "
    , "                   'p.while')."
    , "    :printmode m - Set the print mode to mode 'm'. Valid modes are i, "
    , "                   iv, l, li, liv, L, and La. Quit interactive mode and "
    , "                   then run 'hwhile -h' for more info on print modes."
    , "    :cd dir      - Change the current file search path to 'dir'."
    , "    (Ctrl+D)     - Quit interactive mode."
    ]

welcomeString :: String
welcomeString = "Welcome to HWhile interactive mode. Type ':help' for more " ++
    "information."
