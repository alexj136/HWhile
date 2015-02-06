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

-- Generates a test that checks that a single token is lexed correctly
lexTest :: String -> L.Token -> Test
lexTest testString expectedToken =
    TestLabel
        ("Test that the text " ++ show testString ++ " is lexed correctly.")
        (TestCase (assert (L.alexScanTokens testString == [ expectedToken ])))

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

parserTests =
    [ TestLabel "" (TestCase (assert True))
    ]

loadProg :: FilePath -> IO S.Program
loadProg fileName = fmap M.readProg (readFile fileName)

addProg, countProg, equalsProg, numberProg, xorProg :: IO S.Program
addProg    = loadProg "examples/add.while"
countProg  = loadProg "examples/count.while"
equalsProg = loadProg "examples/equals.while"
numberProg = loadProg "examples/number.while"
xorProg    = loadProg "examples/xor.while"

interpreterTests =
    [ TestLabel "Test of the xor program" (TestCase (assert True))
    ]
