module UnitTests where

import Test.HUnit
import Lexer
import qualified Parser as P
import qualified Syntax as S
import qualified Interpreter as I

main = runTestTT $ TestList $ concat $
    [ lexerTests
    , parserTests
    , interpreterTests
    ]

lexerTests =
    [ TestLabel "Tests that a single TokenConsInf is lexed correctly" (TestCase (assert (alexScanTokens "."     == [(TokenConsInf (0, 0)        )])))
    , TestLabel "Tests that a single TokenOpenBrc is lexed correctly" (TestCase (assert (alexScanTokens "("     == [(TokenOpenBrc (0, 0)        )])))
    , TestLabel "Tests that a single TokenClosBrc is lexed correctly" (TestCase (assert (alexScanTokens ")"     == [(TokenClosBrc (0, 0)        )])))
    , TestLabel "Tests that a single TokenOpenCur is lexed correctly" (TestCase (assert (alexScanTokens "{"     == [(TokenOpenCur (0, 0)        )])))
    , TestLabel "Tests that a single TokenClosCur is lexed correctly" (TestCase (assert (alexScanTokens "}"     == [(TokenClosCur (0, 0)        )])))
    , TestLabel "Tests that a single TokenIsEq    is lexed correctly" (TestCase (assert (alexScanTokens "?="    == [(TokenIsEq    (0, 0)        )])))
    , TestLabel "Tests that a single TokenAssign  is lexed correctly" (TestCase (assert (alexScanTokens ":="    == [(TokenAssign  (0, 0)        )])))
    , TestLabel "Tests that a single TokenNil     is lexed correctly" (TestCase (assert (alexScanTokens "nil"   == [(TokenNil     (0, 0)        )])))
    , TestLabel "Tests that a single TokenSemiCo  is lexed correctly" (TestCase (assert (alexScanTokens ";"     == [(TokenSemiCo  (0, 0)        )])))
    , TestLabel "Tests that a single TokenConsPre is lexed correctly" (TestCase (assert (alexScanTokens "cons"  == [(TokenConsPre (0, 0)        )])))
    , TestLabel "Tests that a single TokenHead    is lexed correctly" (TestCase (assert (alexScanTokens "head"  == [(TokenHead    (0, 0)        )])))
    , TestLabel "Tests that a single TokenHead    is lexed correctly" (TestCase (assert (alexScanTokens "hd"    == [(TokenHead    (0, 0)        )])))
    , TestLabel "Tests that a single TokenTail    is lexed correctly" (TestCase (assert (alexScanTokens "tail"  == [(TokenTail    (0, 0)        )])))
    , TestLabel "Tests that a single TokenTail    is lexed correctly" (TestCase (assert (alexScanTokens "tl"    == [(TokenTail    (0, 0)        )])))
    , TestLabel "Tests that a single TokenWhile   is lexed correctly" (TestCase (assert (alexScanTokens "while" == [(TokenWhile   (0, 0)        )])))
    , TestLabel "Tests that a single TokenDo      is lexed correctly" (TestCase (assert (alexScanTokens "do"    == [(TokenDo      (0, 0)        )])))
    , TestLabel "Tests that a single TokenRead    is lexed correctly" (TestCase (assert (alexScanTokens "read"  == [(TokenRead    (0, 0)        )])))
    , TestLabel "Tests that a single TokenWrite   is lexed correctly" (TestCase (assert (alexScanTokens "write" == [(TokenWrite   (0, 0)        )])))
    , TestLabel "Tests that a single TokenVar     is lexed correctly" (TestCase (assert (alexScanTokens "hello" == [(TokenVar     (0, 0) "hello")])))
    , TestLabel "Tests that a single TokenInt     is lexed correctly" (TestCase (assert (alexScanTokens "101"   == [(TokenInt     (0, 0)   "101")])))
    ]

parserTests =
    [ TestLabel "" (TestCase (assert True))
    ]

interpreterTests =
    [ TestLabel "" (TestCase (assert True))
    ]
