module Main where

import qualified Lexer as L
import qualified Parser as P
import qualified PureSyntax as S
import SugarSyntax
import qualified PureInterpreter as I
import qualified CodeGen as C
import System.Environment
import Data.List (intersperse)

readProg :: String -> SuProgram
readProg = P.parseProg . L.alexScanTokens

readComm :: String -> SuCommand
readComm = P.parseComm . L.alexScanTokens

readExpr :: String -> S.Expression
readExpr = P.parseExpr . L.alexScanTokens

evalFromStr :: String -> String -> S.Expression
evalFromStr argStr progStr =
    I.evalProg (readExpr argStr) (desugarProg (readProg progStr))

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
    , "    [NOTHING] - Display output as a while tree"
    , "    -i        - Display output as an integer. If the output is not a"
    , "                valid integer, 'E' will be displayed"
    , "    -iv       - Display output as an integer. If the output is not a"
    , "                valid integer, it will be displayed as a while tree"
    , "    -l        - Display output as a list of while trees"
    , "    -li       - Display output as a list of integers. Invalid elements"
    , "                will all display as 'E'"
    , "    -liv      - Display output as a list of integers. Invalid elements"
    , "                will all display as while trees"
    ]

-- Print an expression in a certain way according to the given command line
-- argument (see Main.hs for a description of what these should do)
showFlag :: String -> S.Expression -> String
showFlag f exp = case f of
    "-i"   -> case S.parseInt exp of
        Just i  -> show i
        Nothing -> "E"
    "-iv"  -> case S.parseInt exp of
        Just i  -> show i
        Nothing -> show exp
    "-l"   -> show (S.toActualList exp)
    "-li"  -> show (map (S.showIntExp False) (S.toActualList exp))
    "-liv" -> show (map (S.showIntExp True ) (S.toActualList exp))
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
        putStrLn $ showFlag (args !! 0) (evalFromStr (args !! 2) progStr)

    else
        putStrLn "Invalid argument(s) supplied. Run 'hwhile -h' for help."
