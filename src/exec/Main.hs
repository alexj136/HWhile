module Main where

import qualified Data.Map           as M
import qualified Data.Set           as S
import System.Environment (getArgs)
import System.FilePath
import Data.List (intersperse)
import qualified PureSyntax         as PS
import SugarSyntax
import qualified PureInterpreter    as I
import qualified LoggingInterpreter as LI
import qualified Unparser           as U
import HWhileUtils

noArgsMessage :: String
noArgsMessage = "No arguments supplied. Run 'hwhile -h' for help."

badArgsMessage :: String
badArgsMessage = "Invalid argument(s) supplied. Run 'hwhile -h' for help."

helpMessage :: String
helpMessage = concat $ (intersperse "\n") $
    [ "HWhile: a Haskell implementation of the while language, by Alex Jeffery."
    , "Usage:"
    , "    hwhile -h                    - Print this message and exit."
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
    Just "-di"   -> let sfn = \tree -> maybe "E" show $ PS.parseInt tree         in Just (sfn, LI.evalProg sfn)
    Just "-div"  -> let sfn = \tree -> maybe (show tree) show $ PS.parseInt tree in Just (sfn, LI.evalProg sfn)
    Just "-dl"   -> let sfn = show . PS.toHaskellList                            in Just (sfn, LI.evalProg sfn)
    Just "-dli"  -> let sfn = PS.showIntListTree False                           in Just (sfn, LI.evalProg sfn)
    Just "-dliv" -> let sfn = PS.showIntListTree True                            in Just (sfn, LI.evalProg sfn)
    Just "-dL"   -> let sfn = PS.showNestedIntListTree                           in Just (sfn, LI.evalProg sfn)
    Just "-dLa"  -> let sfn = PS.showNestedAtomIntListTree                       in Just (sfn, LI.evalProg sfn)
    Just _       -> Nothing

-- Parse the command structure to pass appropriate arguments to doRun, or quit
-- with an error/help message.
main :: IO ()
main = do
    args <- getArgs
    if (length args) == 0 then do
        putStrLn noArgsMessage
    else if (args !! 0) == "-h" then do
        putStrLn helpMessage
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
        putStrLn badArgsMessage

doRun :: Maybe String -> FilePath -> String -> IO ()
doRun flagStr mainFile argStr =
    let mainFileDir      = takeDirectory mainFile
        mainFileBaseName = takeBaseName mainFile
    in case getShowFunctionAndInterpreterFunction flagStr of
        Nothing           -> putStrLn badArgsMessage
        Just (showFunction, interpreterFunction) -> do
            fileMap <- buildFileMap mainFileDir M.empty $
                S.singleton mainFileBaseName
            result  <- runFromParts mainFileBaseName fileMap argStr interpreterFunction
            putStrLn $ showFunction $ result
