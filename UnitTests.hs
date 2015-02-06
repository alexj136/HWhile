module UnitTests where

import Test.HUnit
import qualified Lexer       as L
import qualified Parser      as P
import qualified Syntax      as S
import qualified Interpreter as I
import qualified CodeGen     as C
import qualified Main        as M

main = runTestTT $ TestList $ concat $
    [ lexerTests
    , parserTests
    , interpreterTests
    ]

makeTest :: Assertable t => String -> t -> Test
makeTest description test = TestLabel description (TestCase (assert test))

--------------------------------------------------------------------------------
--                            Lexer Unit Tests                                --
--------------------------------------------------------------------------------

-- Generates a test that checks that a single token is lexed correctly
lexTest :: String -> L.Token -> Test
lexTest testString expectedToken =
    makeTest
        ("Test that the text " ++ show testString ++ " is lexed correctly.")
        (L.alexScanTokens testString == [ expectedToken ])

lexerTests :: [Test]
lexerTests =
    [ lexTest "."     (L.TokenConsInf (0, 0)        )
    , lexTest "("     (L.TokenOpenBrc (0, 0)        )
    , lexTest ")"     (L.TokenClosBrc (0, 0)        )
    , lexTest "{"     (L.TokenOpenCur (0, 0)        )
    , lexTest "}"     (L.TokenClosCur (0, 0)        )
    , lexTest "?="    (L.TokenIsEq    (0, 0)        )
    , lexTest ":="    (L.TokenAssign  (0, 0)        )
    , lexTest "nil"   (L.TokenNil     (0, 0)        )
    , lexTest ";"     (L.TokenSemiCo  (0, 0)        )
    , lexTest "cons"  (L.TokenConsPre (0, 0)        )
    , lexTest "head"  (L.TokenHead    (0, 0)        )
    , lexTest "hd"    (L.TokenHead    (0, 0)        )
    , lexTest "tail"  (L.TokenTail    (0, 0)        )
    , lexTest "tl"    (L.TokenTail    (0, 0)        )
    , lexTest "while" (L.TokenWhile   (0, 0)        )
    , lexTest "do"    (L.TokenDo      (0, 0)        )
    , lexTest "end"   (L.TokenEnd     (0, 0)        )
    , lexTest "read"  (L.TokenRead    (0, 0)        )
    , lexTest "write" (L.TokenWrite   (0, 0)        )
    , lexTest "hello" (L.TokenVar     (0, 0) "hello")
    , lexTest "101"   (L.TokenInt     (0, 0)   "101")
    ]

--------------------------------------------------------------------------------
--                            Parser Unit Tests                               --
--------------------------------------------------------------------------------

parserTests :: [Test]
parserTests =
    [ TestLabel "" (TestCase (assert True))
    ]

--------------------------------------------------------------------------------
--                          Interpreter Unit Tests                            --
--------------------------------------------------------------------------------

loadProg :: FilePath -> IO S.Program
loadProg fileName = fmap M.readProg (readFile fileName)

addProg, countProg, equalsProg, numberProg, xorProg :: IO S.Program
addProg    = loadProg "examples/add.while"
countProg  = loadProg "examples/count.while"
equalsProg = loadProg "examples/equals.while"
numberProg = loadProg "examples/number.while"
xorProg    = loadProg "examples/xor.while"

-- Run a program obtained through IO with the given input, and compare the
-- output with a given expression
testRun :: String -> IO S.Program -> String -> IO Bool
testRun argString ioProg expectedResultString = do
    prog <- ioProg
    return (I.evalProg argExpr prog == expRes)
    where
        argExpr = P.parseExpr (L.alexScanTokens argString)
        expRes  = P.parseExpr (L.alexScanTokens expectedResultString)

interpreterTests :: [Test]
interpreterTests =
    [ makeTest "Test of the xor program: true , true  -> false" (testRun "((nil.nil).(nil.nil))" xorProg "nil"      )
    , makeTest "Test of the xor program: false, true  -> true " (testRun "(nil.(nil.nil))"       xorProg "(nil.nil)")
    , makeTest "Test of the xor program: true , false -> true " (testRun "((nil.nil).nil)"       xorProg "(nil.nil)")
    , makeTest "Test of the xor program: false, false -> false" (testRun "(nil.nil)"             xorProg "nil"      )
    ]
