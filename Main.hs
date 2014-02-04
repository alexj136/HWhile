module Main where

import qualified Lexer as L
import qualified Parser as P
import qualified Syntax as S
import qualified Interpreter as I
import System.Environment

readProg :: String -> S.Program
readProg = P.parseProg . L.alexScanTokens 

readComm :: String -> S.Command
readComm = P.parseComm . L.alexScanTokens 

readExpr :: String -> S.Expression
readExpr = P.parseExpr . L.alexScanTokens 

evalFromStr :: String -> String -> S.Expression
evalFromStr argStr progStr = I.evalProg (readExpr argStr) (readProg progStr)
    where

main = do
    cmdLnArgs <- getArgs
    progStr <- readFile (cmdLnArgs !! 0)
    putStrLn $ show $ evalFromStr (cmdLnArgs !! 1) progStr
