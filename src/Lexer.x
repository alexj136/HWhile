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
    \.                    { \p s -> SimpleToken (pos p) TkConsInf } -- Infix cons
    \(                    { \p s -> SimpleToken (pos p) TkOpenBrc }
    \)                    { \p s -> SimpleToken (pos p) TkClosBrc }
    \{                    { \p s -> SimpleToken (pos p) TkOpenCur }
    \}                    { \p s -> SimpleToken (pos p) TkClosCur }
    \[                    { \p s -> SimpleToken (pos p) TkOpenSqu }
    \]                    { \p s -> SimpleToken (pos p) TkClosSqu }
    \,                    { \p s -> SimpleToken (pos p) TkComma   }
    \?\=                  { \p s -> SimpleToken (pos p) TkIsEq    }
    \:\=                  { \p s -> SimpleToken (pos p) TkAssign  }
    \:                    { \p s -> SimpleToken (pos p) TkColon   }
    "nil"                 { \p s -> SimpleToken (pos p) TkNil     }
    \;                    { \p s -> SimpleToken (pos p) TkSemiCo  }
    "cons"                { \p s -> SimpleToken (pos p) TkConsPre } -- Prefix cons
    "hd"                  { \p s -> SimpleToken (pos p) TkHead    }
    "head"                { \p s -> SimpleToken (pos p) TkHead    }
    "tl"                  { \p s -> SimpleToken (pos p) TkTail    }
    "tail"                { \p s -> SimpleToken (pos p) TkTail    }
    "while"               { \p s -> SimpleToken (pos p) TkWhile   }
    "switch"              { \p s -> SimpleToken (pos p) TkSwitch  }
    "case"                { \p s -> SimpleToken (pos p) TkCase    }
    "default"             { \p s -> SimpleToken (pos p) TkDefault }
    "if"                  { \p s -> SimpleToken (pos p) TkIf      }
    "else"                { \p s -> SimpleToken (pos p) TkElse    }
    "read"                { \p s -> SimpleToken (pos p) TkRead    }
    "write"               { \p s -> SimpleToken (pos p) TkWrite   }
    $alpha[$alnum \_ \']* { \p s -> TokenVar    (pos p) s         }
    "0"                   { \p s -> TokenInt    (pos p) s         }
    [1-9][$digit]*        { \p s -> TokenInt    (pos p) s         }

{
data Token
    = SimpleToken (Int, Int) STKind
    | TokenVar    (Int, Int) String
    | TokenInt    (Int, Int) String
    deriving Show

data STKind
    = TkConsInf
    | TkOpenBrc
    | TkClosBrc
    | TkOpenCur
    | TkClosCur
    | TkOpenSqu
    | TkClosSqu
    | TkComma  
    | TkColon  
    | TkIsEq   
    | TkAssign 
    | TkNil    
    | TkSemiCo 
    | TkConsPre
    | TkHead   
    | TkTail   
    | TkWhile  
    | TkSwitch 
    | TkCase   
    | TkDefault
    | TkIf     
    | TkElse   
    | TkRead   
    | TkWrite  
    deriving (Show, Eq)

-- The default implementation is not quite sufficient - it is more useful for
-- tokens to be equal regardless of position
instance Eq Token where
    (==) (SimpleToken  _ a) (SimpleToken  _ b) = a == b
    (==) (TokenVar     _ a) (TokenVar     _ b) = a == b
    (==) (TokenInt     _ a) (TokenInt     _ b) = a == b
    (==) _                  _                  = False

-- Get the number of lines into the file that the text produced this token
-- occurred
lineNo :: Token -> Int
lineNo tok = case tok of
    SimpleToken  (x, _) _ -> x
    TokenVar     (x, _) _ -> x
    TokenInt     (x, _) _ -> x

-- Get the number of characters into the line that the text that produced this
-- token occurred
charNo :: Token -> Int
charNo tok = case tok of
    SimpleToken  (_, x) _ -> x
    TokenVar     (_, x) _ -> x
    TokenInt     (_, x) _ -> x

-- Get a string representation of a token for error message purposes
tokStr :: Token -> String
tokStr tok = case tok of
    SimpleToken (_, _) TkConsInf -> "'.'"
    SimpleToken (_, _) TkOpenBrc -> "'('"
    SimpleToken (_, _) TkClosBrc -> "')'"
    SimpleToken (_, _) TkOpenCur -> "'{'"
    SimpleToken (_, _) TkClosCur -> "'}'"
    SimpleToken (_, _) TkOpenSqu -> "'['"
    SimpleToken (_, _) TkClosSqu -> "']'"
    SimpleToken (_, _) TkComma   -> "','"
    SimpleToken (_, _) TkIsEq    -> "'?='"
    SimpleToken (_, _) TkAssign  -> "':='"
    SimpleToken (_, _) TkNil     -> "'nil'"
    SimpleToken (_, _) TkSemiCo  -> "';'"
    SimpleToken (_, _) TkConsPre -> "'cons'"
    SimpleToken (_, _) TkHead    -> "'head'"
    SimpleToken (_, _) TkTail    -> "'tail'"
    SimpleToken (_, _) TkWhile   -> "'while'"
    SimpleToken (_, _) TkIf      -> "'if'"
    SimpleToken (_, _) TkElse    -> "'else'"
    SimpleToken (_, _) TkRead    -> "'read'"
    SimpleToken (_, _) TkWrite   -> "'write'"
    TokenVar    (_, _) s       -> "variable '" ++ s ++ "'"
    TokenInt    (_, _) s       -> "integer '" ++ s ++ "'"

pos :: AlexPosn -> (Int, Int)
pos (AlexPn i j k) = (j, k)
}
