module Main where

import qualified Lexer as L
import qualified Parser as P
import qualified Syntax as S
import qualified Interpreter as I
import System.Environment
import Data.List (intersperse)

readProg :: String -> S.Program
readProg = P.parseProg . L.alexScanTokens 

readComm :: String -> S.Command
readComm = P.parseComm . L.alexScanTokens 

readExpr :: String -> S.Expression
readExpr = P.parseExpr . L.alexScanTokens 

evalFromStr :: String -> String -> S.Expression
evalFromStr argStr progStr = I.evalProg (readExpr argStr) (readProg progStr)
    where

helpMessage = concat $ (intersperse "\n") $
    [ "HWhile: an interpreter for the while language, written in Haskell by Alex Jeffery."
    , "Usage:"
    , "    hwhile -h                    - Print this message and exit."
    , "    hwhile <FLAG> <FILE> <EXPR>  - Run the program in <FILE> with input <EXPR>."
    , "                                   Note that <EXPR> may require surrounding \"double quotes\"."
    , "                                   Program output will be displayed in the format specified by"
    , "                                   the chosen <FLAG>"
    , "Possible flags:"
    , "    [NOTHING] - Display output as a while tree"
    , "    -i        - Display output as an integer. If the output is not a valid integer, 'E' will"
    , "                be displayed"
    , "    -iv       - Display output as an integer. If the output is not a valid integer, it will be"
    , "                displayed as a while tree"
    , "    -l        - Display output as a list of while trees"
    , "    -li       - Display output as a list of integers. Invalid elements will all display as 'E'"
    , "    -liv      - Display output as a list of integers. Invalid elements will all display as"
    , "                while trees."
    ]

-- Print an expression in a certain way according to the given command line
-- argument (see Main.hs for a description of what these should do)
showFlag :: String -> Expression -> String
showFlag f exp = case f of
    "-i"   -> case parseInt exp of
        Just i  -> show i
        Nothing -> "E"
    "-iv"  -> case parseInt exp of
        Just i  -> show i
        Nothing -> show exp
    "-l"   -> show (toActualList exp)
    "-li"  -> show (map (showIntExp False) (toActualList exp))
    "-liv" -> show (map (showIntExp True ) (toActualList exp))
    _      -> "Invalid argument(s) supplied. Run 'hwhile -h' for help."

main = do
    args <- getArgs

    if (length args) == 0 then do
        putStrLn "No arguments supplied. Run 'hwhile -h' for help."

    else if (args !! 0) == "-h" then do
        putStrLn helpMessage

    else if (length args) == 2 then do
        progStr <- readFile (args !! 0)
        putStrLn $ show (evalFromStr (args !! 1) progStr)

    else if (length args) == 3 then do
        progStr <- readFile (args !! 1)
        putStrLn $ S.showFlag (args !! 0) (evalFromStr (args !! 2) progStr)

    else
        putStrLn "Invalid argument(s) supplied. Run 'hwhile -h' for help."
