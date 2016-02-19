module Main where

import qualified Data.Map        as M
import qualified Data.Set        as S
import System.Environment (getArgs)
import System.FilePath
import Data.List (intersperse)
import qualified Lexer           as L
import qualified Parser          as P
import qualified PureSyntax      as PS
import SugarSyntax
import qualified PureInterpreter as I
import qualified Unparser        as U
import HWhileUtils

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
showFlag :: String -> PS.Expression -> String
showFlag f exp = case f of
    "-i"   -> case PS.parseInt exp of
        Just i  -> show i
        Nothing -> "E"
    "-iv"  -> case PS.parseInt exp of
        Just i  -> show i
        Nothing -> show exp
    "-l"   -> show (PS.toActualList exp)
    "-li"  -> show (map (PS.showIntExp False) (PS.toActualList exp))
    "-liv" -> show (map (PS.showIntExp True ) (PS.toActualList exp))
    _      -> "Invalid argument(s) supplied. Run 'hwhile -h' for help."

main = do
    args <- getArgs

    if (length args) == 0 then do
        putStrLn "No arguments supplied. Run 'hwhile -h' for help."

    else if (args !! 0) == "-h" then do
        putStrLn helpMessage

    else if (length args) == 2 then do
        let mainFile         = args !! 0
        let mainFileDir      = takeDirectory mainFile
        let mainFileBaseName = takeBaseName mainFile
        let argStr           = args !! 1
        fileMap <- buildFileMap mainFileDir M.empty
                (S.singleton mainFileBaseName)
        putStrLn $ show
                (runFromParts mainFileBaseName fileMap argStr)

    else if (length args) == 3 then do
        let flagStr  = args !! 0
        let mainFile = args !! 1
        let mainFileDir      = takeDirectory mainFile
        let mainFileBaseName = takeBaseName mainFile
        let argStr   = args !! 2
        fileMap <- buildFileMap mainFileDir M.empty
                (S.singleton mainFileBaseName)
        putStrLn $ showFlag flagStr
                (runFromParts mainFileBaseName fileMap argStr)

    else
        putStrLn "Invalid argument(s) supplied. Run 'hwhile -h' for help."
