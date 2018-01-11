module Main where

import qualified Data.Map        as M
import qualified Data.Set        as S
import qualified Lexer           as L
import qualified SourceParser    as SP
import qualified PureSyntax      as PS
import qualified InterSyntax     as IS
import qualified SugarSyntax     as SS
import qualified PureInterpreter as I
import qualified DesugarSI       as SI
import qualified DesugarIP       as IP

import System.Exit
import Control.Monad.Except

addProg, countProg, equalsProg, numberProg, xorProg, macroProg, casesProg
    :: ExceptT String IO IS.InProgram
addProg    = SI.loadProg "examples" "add"    []
countProg  = SI.loadProg "examples" "count"  []
equalsProg = SI.loadProg "examples" "equals" []
numberProg = SI.loadProg "examples" "number" []
xorProg    = SI.loadProg "examples" "xor"    []
macroProg  = SI.loadProg "examples" "macro"  []
casesProg  = SI.loadProg "examples" "cases"  []

-- Run a program obtained through IO with the given input, and compare the
-- output with a given expression
testRun :: String -> ExceptT String IO IS.InProgram -> String -> IO Bool
testRun argumentString eioProg expectedResultString = do
    prog <- errorIfExcepts eioProg
    argumentExpr <- errorIfExcepts $
        SP.parseLVal (L.scan argumentString       "+TEST+")
    expectedRes  <- errorIfExcepts $
        SP.parseLVal (L.scan expectedResultString "+TEST+")
    return (I.evalProg argumentExpr (IP.desugarProg prog) == expectedRes)

test :: String -> IO Bool -> IO (String, Bool)
test desc ioRes = do
    res <- ioRes
    return (desc, res)

errorIfExcepts :: ExceptT String IO a -> IO a
errorIfExcepts excA = do
    eithA <- runExceptT excA
    case eithA of { Left err -> error err ; Right a -> return a }

main :: IO ExitCode
main = do
    tests <- sequence
        -- xor program
        [ test "Test xor program: t, t -> f"
            (testRun "<<nil.nil>.<nil.nil>>" xorProg    "nil"      )
        , test "Test xor program: f, t -> t"
            (testRun "<nil.<nil.nil>>"       xorProg    "<nil.nil>")
        , test "Test xor program: t, f -> t"
            (testRun "<<nil.nil>.nil>"       xorProg    "<nil.nil>")
        , test "Test xor program: f, f -> f"
            (testRun "<nil.nil>"             xorProg    "nil"      )

        -- add program
        , test "Test add program: 3 + 4 -> 7"
            (testRun "<3.4>"                 addProg    "7"        )
        , test "Test add program: 0 + 0 -> 0"
            (testRun "<0.0>"                 addProg    "0"        )

        -- macro program
        , test "Test macro program: 10, 20 -> 40"
            (testRun "<10.20>"               macroProg  "40"       )
        , test "Test macro program: 0, 0 -> 0"
            (testRun "<0.0>"                 macroProg  "0"        )

        -- equals program
        , test "Test equals program: [1, 2], [1, 2] -> true"
            (testRun "<[1, 2].[1, 2]>"       equalsProg "true"     )
        , test "Test equals program: [1, 1], [1, 2] -> false"
            (testRun "<[1, 1].[1, 2]>"       equalsProg "false"    )

        -- cases program
        , test "Test cases program: 0 -> 0"
            (testRun "0"                     casesProg  "0"        )
        , test "Test cases program: 1 -> 1"
            (testRun "1"                     casesProg  "1"        )
        , test "Test cases program: 2 -> 2"
            (testRun "2"                     casesProg  "2"        )
        , test "Test cases program: 3 -> 3"
            (testRun "3"                     casesProg  "3"        )
        , test "Test cases program: 4 -> 3"
            (testRun "4"                     casesProg  "3"        )

        -- number program
        , test "Test number program: 3 -> true"
            (testRun "3"                     numberProg "true"     )
        , test "Test number program: 0 -> true"
            (testRun "0"                     numberProg "true"     )
        , test "Test number program: [1, 1] -> false"
            (testRun "[1, 1]"                numberProg "false"    )

        -- count program
        , test "Test count program: [] -> 0"
            (testRun "[]"                    countProg  "0"        )
        , test "Test count program: [0] -> 0"
            (testRun "[0]"                   countProg  "0"        )
        , test "Test count program: [0, 0] -> 0"
            (testRun "[0, 0]"                countProg  "0"        )
        , test "Test count program: [0, 1] -> 1"
            (testRun "[0, 1]"                countProg  "1"        )
        , test "Test count program: [0, 1, 2, 3, 4, 5] -> 15"
            (testRun "[0, 1, 2, 3, 4, 5]"    countProg  "15"       )
        ]
    if all snd tests then
        exitSuccess
    else do
        putStr $ concat $ map ((\s ->"FAILED: " ++ s ++ "\n"). fst)
            (filter (not . snd) tests)
        exitFailure
