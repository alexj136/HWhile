module Main where

import qualified Data.Map        as M
import qualified Data.Set        as S
import System.Exit
import qualified Lexer           as L
import qualified Parser          as P
import qualified PureSyntax      as PS
import qualified SugarSyntax     as SS
import qualified PureInterpreter as I
import qualified HWhileUtils     as H

loadProg :: FilePath -> IO PS.Program
loadProg fileName = do
    fileMap <- H.buildFileMap "examples" M.empty (S.singleton fileName)
    return $ SS.desugarProg fileMap (fileMap M.! fileName)

addProg, countProg, equalsProg, numberProg, xorProg :: IO PS.Program
addProg    = loadProg "add"
countProg  = loadProg "count"
equalsProg = loadProg "equals"
numberProg = loadProg "number"
xorProg    = loadProg "xor"

-- Run a program obtained through IO with the given input, and compare the
-- output with a given expression
testRun :: String -> IO PS.Program -> String -> IO Bool
testRun argString ioProg expectedResultString = do
    prog <- ioProg
    return (I.evalProg argExpr prog == expRes)
    where
        argExpr = P.parseExpr (L.scan "+TEST+" argString)
        expRes  = P.parseExpr (L.scan "+TEST+" expectedResultString)

test :: String -> IO Bool -> IO (String, Bool)
test desc ioRes = do
    res <- ioRes
    return (desc, res)

main :: IO ExitCode
main = do
    tests <- sequence
        [ test "Test xor program: t, t -> f"
            (testRun "<<nil.nil>.<nil.nil>>" xorProg "nil"      )
        , test "Test xor program: f, t -> t"
            (testRun "<nil.<nil.nil>>"       xorProg "<nil.nil>")
        , test "Test xor program: t, f -> t"
            (testRun "<<nil.nil>.nil>"       xorProg "<nil.nil>")
        , test "Test xor program: f, f -> f"
            (testRun "<nil.nil>"             xorProg "nil"      )
        , test "Test add program: 3 + 4 -> 7"
            (testRun "<3.4>"                 addProg "7"        )
        , test "Test add program: 0 + 0 -> 0"
            (testRun "<0.0>"                 addProg "0"        )
        ]
    if all snd tests then
        exitSuccess
    else do
        putStr $ concat $ map ((\s ->"FAILED: " ++ s ++ "\n"). fst)
            (filter (not . snd) tests)
        exitFailure
