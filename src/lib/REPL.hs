module REPL (REPL, runREPL, emptyREPLState) where

import qualified Lexer        as L
import qualified SourceParser as SP
import qualified DesugarSI    as DS
import PureSyntax
import InterSyntax
import SugarSyntax
import PureInterpreter (evalExpr, Store)

import Text.Read (readMaybe)
import Data.Either (lefts)
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

data DebugOp = WhileRead Name ETree | WhileWrite Name | Message String
    deriving (Show, Eq, Ord)

type REPLState =
    ( Store                         -- The store for interactive execution
    , [Either InCommand DebugOp]    -- A loaded program that can be executed.
                                    -- This is a list of either a while command
                                    -- to execute, or a debugger-level
                                    -- instruction to carry out.
    , S.Set Breakpoint              -- Breakpoints that we pause at
    , ETree -> String               -- Tree printing function
    , FilePath                      -- Current file search path
    )

emptyREPLState :: REPLState
emptyREPLState = (M.empty, [], S.empty, show, ".")

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
    , ("store"    , store    )
    , ("step"     , step     )
    , ("break"    , undefined)
    , ("delbreak" , undefined)
    ]

help :: [String] -> REPL ()
help _ = liftIO $ putStrLn helpString

load :: [String] -> REPL ()
load args
    | length args <= 1 =
        replPutStrLn $ "Please supply a single while program name, and an " ++
            "argument literal (e.g. <nil.nil>)."
    | otherwise        = do
        path    <- getPath
        progTry <- loadProg path (head args)
        argTry  <- parseLVal (intercalate " " (tail args))
        case (progTry, argTry) of
            (Left  err1, Left  err2) -> do replPutStrLn err1 ; replPutStrLn err2
            (_         , Left  err ) -> replPutStrLn err
            (Left  err , _         ) -> replPutStrLn err
            (Right prog, Right arg ) -> do
                replPutStrLn $ "Program '" ++ (head args) ++ "' loaded."
                case prog of
                    InProgram _ rd blk wr ->
                        putProg $ [Right (WhileRead rd arg)] ++ map Left blk ++
                            [Right (WhileWrite wr)]

run :: [String] -> REPL ()
run args
    | length args /= 0 = replPutStrLn $
        "Error: did not expect '" ++ intercalate " " args ++ "'."
    | otherwise        = do
        store   <- getStore
        bps     <- getBreakpoints
        blk     <- getProg
        printFn <- getPrintFn
        let (store', blk', msg) = runToBreakpoint store bps blk printFn
        putStore store'
        putProg blk'
        replPutStrLn msg

printmode :: [String] -> REPL ()
printmode args
    | length args /= 1 =
        replPutStrLn "Please supply a single print mode string."
    | otherwise        = case head args of
        "i"   -> putPrintFn $ \tree -> maybe "E" show $ parseInt tree
        "iv"  -> putPrintFn $ \tree -> maybe (show tree) show $ parseInt tree
        "l"   -> putPrintFn $ show . toHaskellList
        "li"  -> putPrintFn $ showIntListTree False
        "liv" -> putPrintFn $ showIntListTree True
        "L"   -> putPrintFn $ showNestedIntListTree
        "La"  -> putPrintFn $ showNestedAtomIntListTree
        _     -> replPutStrLn $ "Error - valid modes are i, iv, l, li, liv, " ++
            "L, and La. Quit interactive mode and then run 'hwhile -h' for " ++
            "more information."

cd :: [String] -> REPL ()
cd args
    | length args /= 1 = replPutStrLn "Please supply a single directory path."
    | otherwise        = putPath (head args)

store :: [String] -> REPL ()
store args
    | length args /= 0 = replPutStrLn $
        "Error: did not expect '" ++ intercalate " " args ++ "'."
    | otherwise        = do
        store <- getStore
        printFn <- getPrintFn
        let output = intercalate "\n" $
                map (\(Name (fp, n), tree) ->
                        "(" ++ fp ++ ") " ++ n ++ " := " ++ printFn tree) $
                M.assocs store
        replPutStrLn output

step :: [String] -> REPL ()
step args
    | length args /= 0 = replPutStrLn $
        "Error: did not expect '" ++ intercalate " " args ++ "'."
    | otherwise        = do
        store   <- getStore
        blk     <- getProg
        printFn <- getPrintFn
        let (store', blk', msg) = doStep store blk printFn
        putStore store'
        putProg blk'
        replPutStrLn msg

break :: [String] -> REPL ()
break args
    | length args /= 1 = replPutStrLn "Please supply a line number."
    | otherwise        = do
        fp <- getCurrentFilePath
        --putBreakpoint (fp
        undefined

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

runToBreakpoint :: Store -> S.Set Breakpoint -> [Either InCommand DebugOp] ->
    (ETree -> String) -> (Store, [Either InCommand DebugOp], String)
runToBreakpoint store bps blk printFn =
    let res@(store', blk', msg) = doStep store blk printFn in
        case blk' of
            []              -> res
            Right _     : _ -> runToBreakpoint store' bps blk' printFn
            Left inComm : _ -> case info inComm of
                Info i ->
                    if S.member i bps then
                        res
                    else
                        runToBreakpoint store' bps blk' printFn

doStep :: Store -> [Either InCommand DebugOp] -> (ETree -> String) ->
    (Store, [Either InCommand DebugOp], String)
doStep store []            printFn = (store, [], "No program loaded.")
doStep store (comm : rest) printFn = case comm of
    Left (InAssign _ n e) -> let evalE = evalExpr store e in
        (M.insert n evalE store, rest, show n ++ " := " ++ printFn evalE)
    Left (InWhile  _ gd blk) -> case evalExpr store gd of
        ENil -> (store, rest, "Skipped or exited while-loop.")
        _    -> (store, (map Left blk) ++ (comm : rest),
            "while " ++ show gd ++ " { ...\nEntered or re-entered while-loop.")
    Left (InIfElse _ gd tb fb) -> case evalExpr store gd of
        ENil -> (store, (map Left fb) ++ rest,
            "if " ++ show gd ++ " { ...\nTook if-branch.")
        _    -> (store, (map Left tb) ++ rest,
            "if " ++ show gd ++ " { ...\nTook else-branch or skipped.")
    Left (InSwitch _ gd [] def) ->
        (store, (map Left def) ++ rest,
            "switch " ++ show gd ++ " { ... \nTook default case.")
    Left (InSwitch i gd ((e, blk) : cases) def) ->
        if evalExpr store gd == evalExpr store e then
            (store, (map Left blk) ++ rest,
                "switch " ++ show gd ++ " { ...\n    case " ++ show e ++
                ": ...\nTook switch-case.")
        else
            (store, Left (InSwitch i gd cases def) : rest,
                "switch " ++ show gd ++ " { ...\n    case " ++ show e ++
                ": ...\nSkipped switch-case.")
    Right (WhileRead n arg) -> (M.insert n arg store, rest, "read " ++
        show n ++ " := " ++ printFn arg)
    Right (WhileWrite n) -> (store, rest, "wrote " ++ show n ++ " := " ++
        (printFn (M.findWithDefault ENil n store)))
    Right (Message m) -> (store, rest, m)

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

putStore :: Store -> REPL ()
putStore st = lift $ modify $
    \(_, p, bps, pf, fp) -> (st, p, bps, pf, fp)

getFromStore :: Name -> REPL (Maybe ETree)
getFromStore name = do store <- getStore ; return $ M.lookup name store

putInStore :: Name -> ETree -> REPL ()
putInStore n t = do store <- getStore ; putStore $ M.insert n t store

getProg :: REPL [Either InCommand DebugOp]
getProg = do (_, p, _, _, _) <- lift get ; return p

putProg :: [Either InCommand DebugOp] -> REPL ()
putProg blk = lift $ modify $ \(st, _, bps, pf, fp) -> (st, blk, bps, pf, fp)

getCurrentFilePath :: REPL (Maybe FilePath)
getCurrentFilePath = do
    blk <- getProg
    case lefts blk of
        [] -> return Nothing
        cs -> return $ Just $ case info (head cs) of Info (fp, line) -> fp

getBreakpoints :: REPL (S.Set Breakpoint)
getBreakpoints = do (_, _, bps, _, _) <- lift get ; return bps

putBreakpoints :: S.Set Breakpoint -> REPL ()
putBreakpoints bps = lift $ modify $
    \(st, mp, _, pf, fp) -> (st, mp, bps, pf, fp)

putBreakpoint :: Breakpoint -> REPL ()
putBreakpoint bp = do bps <- getBreakpoints ; putBreakpoints $ S.insert bp bps

delBreakpoint :: Breakpoint -> REPL ()
delBreakpoint bp = do bps <- getBreakpoints ; putBreakpoints $ S.delete bp bps

isBreakpoint :: Breakpoint -> REPL Bool
isBreakpoint bp = do bps <- getBreakpoints ; return $ S.member bp bps

getPrintFn :: REPL (ETree -> String)
getPrintFn = do (_, _, _, pf, _) <- lift get ; return pf

putPrintFn :: (ETree -> String) -> REPL ()
putPrintFn pf = lift $ modify $
    \(st, mp, bps, _, fp) -> (st, mp, bps, pf, fp)

getPath :: REPL FilePath
getPath = do (_, _, _, _, fp) <- lift get ; return fp

putPath :: String -> REPL ()
putPath fp = lift $ modify $
    \(st, mp, bps, pf, _) -> (st, mp, bps, pf, fp)

--------------------------------------------------------------------------------
-- Parser helpers in the REPL monad
--------------------------------------------------------------------------------

parseExpr :: String -> REPL (Either String Expression)
parseExpr = lift . lift . runExceptT . SP.parseExpr .
    (flip L.scan "<interactive>")

parseComm :: String -> REPL (Either String SuCommand)
parseComm = lift . lift . runExceptT . SP.parseComm .
    (flip L.scan "<interactive>")

parseLVal :: String -> REPL (Either String ETree)
parseLVal = lift . lift . runExceptT . SP.parseLVal .
    (flip L.scan "<interactive>")

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
    , "    <EXPR>         - Evaluate a while expression."
    , "    <COMM>         - Execute a while command."
    , "    :help          - Print this message."
    , "    :load p <EXPR> - Load a while program 'p' (i.e. from the file "
    , "                     'p.while') for execution with argument <EXPR>."
    , "    :run           - Run the loaded program up until the next "
    , "                     breakpoint."
    , "    :step          - Step through a single line of the loaded program."
    , "    :store         - Print the current store contents."
    , "    :printmode m   - Set the print mode to mode 'm'. Valid modes are i, "
    , "                     iv, l, li, liv, L, and La. Quit interactive mode"
    , "                     and then run 'hwhile -h' for more info on print "
    , "                     modes."
    , "    :cd dir        - Change the current file search path to 'dir'."
    , "    (Ctrl+D)       - Quit interactive mode."
    ]

welcomeString :: String
welcomeString = "Welcome to HWhile interactive mode. Type ':help' for more " ++
    "information."
