{
module Lexer where
}

%wrapper "posn"

$digit = 0-9
$lower = a-z
$upper = A-Z
$alpha = [$lower $upper]
$alnum = [$alpha $digit]

tokens :-
    $white+               ; -- Ignore whitespace
    \#.*\n                ; -- Ignore the rest of a line after '#'
    \.                    { \p s -> TokenConsInf (pos p)   } -- Infix cons
    \(                    { \p s -> TokenOpenBrc (pos p)   }
    \)                    { \p s -> TokenClosBrc (pos p)   }
    \{                    { \p s -> TokenOpenCur (pos p)   }
    \}                    { \p s -> TokenClosCur (pos p)   }
    \?\=                  { \p s -> TokenIsEq    (pos p)   }
    \:\=                  { \p s -> TokenAssign  (pos p)   }
    "nil"                 { \p s -> TokenNil     (pos p)   }
    \;                    { \p s -> TokenSemiCo  (pos p)   }
    "cons"                { \p s -> TokenConsPre (pos p)   } -- Prefix cons
    "hd"                  { \p s -> TokenHead    (pos p)   }
    "head"                { \p s -> TokenHead    (pos p)   }
    "tl"                  { \p s -> TokenTail    (pos p)   }
    "tail"                { \p s -> TokenTail    (pos p)   }
    "while"               { \p s -> TokenWhile   (pos p)   }
    "do"                  { \p s -> TokenDo      (pos p)   }
    "read"                { \p s -> TokenRead    (pos p)   }
    "write"               { \p s -> TokenWrite   (pos p)   }
    $alpha[$alnum \_ \']* { \p s -> TokenVar     (pos p) s }
    "0"                   { \p s -> TokenInt     (pos p) s }
    [1-9][$digit]*        { \p s -> TokenInt     (pos p) s }

{
data Token
    = TokenConsInf (Int, Int)
    | TokenOpenBrc (Int, Int)
    | TokenClosBrc (Int, Int)
    | TokenOpenCur (Int, Int)
    | TokenClosCur (Int, Int)
    | TokenIsEq    (Int, Int)
    | TokenAssign  (Int, Int)
    | TokenNil     (Int, Int)
    | TokenSemiCo  (Int, Int)
    | TokenConsPre (Int, Int)
    | TokenHead    (Int, Int)
    | TokenTail    (Int, Int)
    | TokenWhile   (Int, Int)
    | TokenDo      (Int, Int)
    | TokenRead    (Int, Int)
    | TokenWrite   (Int, Int)
    | TokenVar     (Int, Int) String
    | TokenInt     (Int, Int) String
    deriving (Show, Eq)

-- Get the number of lines into the file that the text produced this token
-- occurred
lineNo :: Token -> Int
lineNo tok = case tok of
    TokenConsInf (x, _)   -> x
    TokenOpenBrc (x, _)   -> x
    TokenClosBrc (x, _)   -> x
    TokenOpenCur (x, _)   -> x
    TokenClosCur (x, _)   -> x
    TokenIsEq    (x, _)   -> x
    TokenAssign  (x, _)   -> x
    TokenNil     (x, _)   -> x
    TokenSemiCo  (x, _)   -> x
    TokenConsPre (x, _)   -> x
    TokenHead    (x, _)   -> x
    TokenTail    (x, _)   -> x
    TokenWhile   (x, _)   -> x
    TokenDo      (x, _)   -> x
    TokenRead    (x, _)   -> x
    TokenWrite   (x, _)   -> x
    TokenVar     (x, _) _ -> x
    TokenInt     (x, _) _ -> x

-- Get the number of characters into the line that the text that produced this
-- token occurred
charNo :: Token -> Int
charNo tok = case tok of
    TokenConsInf (_, x)   -> x
    TokenOpenBrc (_, x)   -> x
    TokenClosBrc (_, x)   -> x
    TokenOpenCur (_, x)   -> x
    TokenClosCur (_, x)   -> x
    TokenIsEq    (_, x)   -> x
    TokenAssign  (_, x)   -> x
    TokenNil     (_, x)   -> x
    TokenSemiCo  (_, x)   -> x
    TokenConsPre (_, x)   -> x
    TokenHead    (_, x)   -> x
    TokenTail    (_, x)   -> x
    TokenWhile   (_, x)   -> x
    TokenDo      (_, x)   -> x
    TokenRead    (_, x)   -> x
    TokenWrite   (_, x)   -> x
    TokenVar     (_, x) _ -> x
    TokenInt     (_, x) _ -> x

-- Get a string representation of a token for error message purposes
tokStr :: Token -> String
tokStr tok = case tok of
    TokenConsInf (_, _)   -> "'.'"
    TokenOpenBrc (_, _)   -> "'('"
    TokenClosBrc (_, _)   -> "')'"
    TokenOpenCur (_, _)   -> "'{'"
    TokenClosCur (_, _)   -> "'}'"
    TokenIsEq    (_, _)   -> "'?='"
    TokenAssign  (_, _)   -> "':='"
    TokenNil     (_, _)   -> "'nil'"
    TokenSemiCo  (_, _)   -> "';'"
    TokenConsPre (_, _)   -> "'cons'"
    TokenHead    (_, _)   -> "'head'"
    TokenTail    (_, _)   -> "'tail'"
    TokenWhile   (_, _)   -> "'while'"
    TokenDo      (_, _)   -> "'do'"
    TokenRead    (_, _)   -> "'read'"
    TokenWrite   (_, _)   -> "'write'"
    TokenVar     (_, _) s -> "variable '" ++ s ++ "'"
    TokenInt     (_, _) s -> "integer '" ++ s ++ "'"

pos :: AlexPosn -> (Int, Int)
pos (AlexPn i j k) = (j, k)
}
