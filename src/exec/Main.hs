module Main where

import qualified PureSyntax         as PS
import Lexer (scan)
import SourceParser (parseLVal)
import SugarSyntax
import DesugarSI
import qualified DesugarIP          as IP
import qualified PureInterpreter    as I
import qualified LoggingInterpreter as LI
import Unparser
import REPL
import qualified Data.Map           as M
import qualified Data.Set           as S
import System.Exit
import System.Environment (getArgs)
import System.FilePath
import Data.List (intersperse)
import Control.Monad.Except
import Control.Monad.State.Strict (runStateT)

noArgsMessage :: String
noArgsMessage = "No arguments supplied. Run 'hwhile -h' for help."

badArgsMessage :: String
badArgsMessage = "Invalid argument(s) supplied. Run 'hwhile -h' for help."

helpMessage :: String
helpMessage = concat $ (intersperse "\n") $
    [ "HWhile: a Haskell implementation of the while language, by Alex Jeffery."
    , "Usage:"
    , "    hwhile -h                    - Print this message and exit."
    , "    hwhile -r                    - Enter interactive mode."
    , "    hwhile <FLAG> <FILE> <EXPR>  - Run the program in <FILE> with input"
    , "                                   <EXPR>. Note that <EXPR> may require"
    , "                                   surrounding \"double quotes\"."
    , "                                   Program output will be displayed in"
    , "                                   the format specified by the chosen"
    , "                                   <FLAG>."
    , "Possible flags:"
    , "    [NOTHING] - Display output as a while tree."
    , "    -i        - Display output as an integer. If the output is not a"
    , "                valid integer, 'E' will be displayed."
    , "    -iv       - Display output as an integer. If the output is not a"
    , "                valid integer, it will be displayed as a while tree."
    , "    -l        - Display output as a list of while trees."
    , "    -li       - Display output as a list of integers. Invalid elements"
    , "                will all display as 'E'."
    , "    -liv      - Display output as a list of integers. Invalid elements"
    , "                will all display as while trees."
    , "    -L        - Display output as a nested list of integers. Valid"
    , "                integers will display as such, and invalid integers will"
    , "                display as nested lists of integers."
    , "    -La       - Display output as a nested list of atoms and integers."
    , "                Valid atoms will display as such, invalid atoms that are"
    , "                valid integers will display as integers, and invalid"
    , "                integers will display as nested lists of atoms and"
    , "                integers."
    , "    -d        - Display outputs as while trees, with debugging log"
    , "    -di       - Display outputs as integers, with debugging log. If"
    , "                outputs are not valid integers, 'E' will be displayed."
    , "    -div      - Display outputs as integers. If outputs are not valid"
    , "                integers, they will display as while trees."
    , "    -dl       - Display outputs as lists of while trees, with debugging"
    , "                log."
    , "    -dli      - Display outputs as lists of integers, with debugging"
    , "                log. Invalid elements will display as 'E'."
    , "    -dliv     - Display outputs as lists of integers, with debugging"
    , "                log. Invalid elements will display as while trees."
    , "    -dL       - Display outputs as nested lists of integers, with"
    , "                debugging log. Valid integers will display as such, and"
    , "                invalid integers will display as nested lists of"
    , "                integers."
    , "    -dLa      - Display outputs as nested lists of atoms and integers,"
    , "                with debugging log. Valid atoms will display as such,"
    , "                invalid atoms that are valid integers will display as"
    , "                integers, and invalid integers will display as nested"
    , "                lists of atoms and integers."
    , "    -u        - Run the unparser on the given program. In this mode, no"
    , "                argument expression should be supplied."
    ]

-- Compute a function to display an expression in a certain way according to the
-- given command line argument.
getShowFunctionAndInterpreterFunction ::
    Maybe String ->
    Maybe (PS.ETree -> String, PS.ETree -> PS.Program -> IO PS.ETree)
getShowFunctionAndInterpreterFunction flagStr = case flagStr of
    Nothing      -> Just (show                                               , \t p -> return (I.evalProg t p))
    Just "-i"    -> Just (\tree -> maybe "E" show $ PS.parseInt tree         , \t p -> return (I.evalProg t p))
    Just "-iv"   -> Just (\tree -> maybe (show tree) show $ PS.parseInt tree , \t p -> return (I.evalProg t p))
    Just "-l"    -> Just (show . PS.toHaskellList                            , \t p -> return (I.evalProg t p))
    Just "-li"   -> Just (PS.showIntListTree False                           , \t p -> return (I.evalProg t p))
    Just "-liv"  -> Just (PS.showIntListTree True                            , \t p -> return (I.evalProg t p))
    Just "-L"    -> Just (PS.showNestedIntListTree                           , \t p -> return (I.evalProg t p))
    Just "-La"   -> Just (PS.showNestedAtomIntListTree                       , \t p -> return (I.evalProg t p))
    Just "-d"    -> let sfn = show                                               in Just (sfn, LI.evalProg sfn)
    Just "-di"   -> let sfn = \tree -> maybe "E" show $ PS.parseInt tree         in Just (sfn, LI.evalProg sfn)
    Just "-div"  -> let sfn = \tree -> maybe (show tree) show $ PS.parseInt tree in Just (sfn, LI.evalProg sfn)
    Just "-dl"   -> let sfn = show . PS.toHaskellList                            in Just (sfn, LI.evalProg sfn)
    Just "-dli"  -> let sfn = PS.showIntListTree False                           in Just (sfn, LI.evalProg sfn)
    Just "-dliv" -> let sfn = PS.showIntListTree True                            in Just (sfn, LI.evalProg sfn)
    Just "-dL"   -> let sfn = PS.showNestedIntListTree                           in Just (sfn, LI.evalProg sfn)
    Just "-dLa"  -> let sfn = PS.showNestedAtomIntListTree                       in Just (sfn, LI.evalProg sfn)
    Just _       -> Nothing

main :: IO ()
main = do
    result <- runExceptT exceptMain
    case result of
        Left errorMsg -> do putStrLn errorMsg ; exitFailure
        Right ()      -> exitSuccess

-- Parse the command structure to pass appropriate arguments to doRun, or quit
-- with an error/help message.
exceptMain :: ExceptT String IO ()
exceptMain = do
    args <- lift getArgs
    if (length args) == 0 then do
        lift $ putStrLn noArgsMessage
    else if (args !! 0) == "-r" then do
        lift $ runStateT runREPL emptyREPLState
        return ()
    else if (args !! 0) == "-h" then do
        lift $ putStrLn helpMessage
    else if (length args) == 2 && (args !! 0) == "-u" then do
        let mainFile = args !! 1
        doUnparse mainFile
    else if (length args) == 2 then do
        let mainFile         = args !! 0
        let argStr           = args !! 1
        doRun Nothing mainFile argStr
    else if (length args) == 3 then do
        let flagStr  = args !! 0
        let mainFile = args !! 1
        let argStr   = args !! 2
        doRun (Just flagStr) mainFile argStr
    else
        lift $ putStrLn badArgsMessage

doUnparse :: FilePath -> ExceptT String IO ()
doUnparse mainFile =
    let mainFileDir      = takeDirectory mainFile
        mainFileBaseName = takeBaseName mainFile
    in do
        prog <- loadProg mainFileDir mainFileBaseName []
        maybe (lift (putStrLn "E")) (lift . putStrLn)
            (PS.showProgramTree (unparse (IP.desugarProg prog)))

doRun :: Maybe String -> FilePath -> String -> ExceptT String IO ()
doRun flagStr mainFile argStr =
    let mainFileDir      = takeDirectory mainFile
        mainFileBaseName = takeBaseName mainFile
    in case getShowFunctionAndInterpreterFunction flagStr of
        Nothing -> lift $ putStrLn badArgsMessage
        Just (showFunction, interpreterFunction) -> do
            result <- runFromParts mainFileDir mainFileBaseName argStr
                interpreterFunction
            lift $ putStrLn $ showFunction $ result

-- Run a program given the search path, file path, argument string and
-- interpreting function
runFromParts ::
    FilePath                                -> -- The search path file
    FilePath                                -> -- The main file to run
    String                                  -> -- The argument string
    (PS.ETree -> PS.Program -> IO PS.ETree) -> -- The interpreting function
    ExceptT String IO PS.ETree                 -- The result of the execution
runFromParts dir fileBaseName argStr interpret = do
    prog <- loadProg dir fileBaseName []
    let argTokens  = scan argStr "+IMPL+"
    argTree <- parseLVal argTokens
    lift $ interpret argTree (IP.desugarProg prog)
