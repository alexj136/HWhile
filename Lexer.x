{
module Lexer where
}

%wrapper "basic"

$digit = 0-9
$lower = a-z
$upper = A-Z
$alpha = [$lower $upper]
$alnum = [$alpha $digit]

tokens :-
    $white+         ;
    \.              { \s -> TokenConsInf } -- Infix cons
    \(              { \s -> TokenOpenBrc }
    \)              { \s -> TokenClosBrc }
    \{              { \s -> TokenOpenCur }
    \}              { \s -> TokenClosCur }
    \?\=            { \s -> TokenIsEq    }
    \:\=            { \s -> TokenAssign  }
    "nil"           { \s -> TokenNil     }
    \;              { \s -> TokenSemiCo  }
    "cons"          { \s -> TokenConsPos } -- Prefix cons
    "hd"            { \s -> TokenHead    }
    "head"          { \s -> TokenHead    }
    "tl"            { \s -> TokenTail    }
    "tail"          { \s -> TokenTail    }
    "while"         { \s -> TokenWhile   }
    "do"            { \s -> TokenDo      }
    "read"          { \s -> TokenRead    }
    "write"         { \s -> TokenWrite   }
    [$alnum \_ \']+ { \s -> TokenVar s   }

{
data Token
    = TokenConsInf
    | TokenOpenBrc
    | TokenClosBrc
    | TokenOpenCur
    | TokenClosCur
    | TokenIsEq
    | TokenAssign
    | TokenNil
    | TokenSemiCo
    | TokenConsPos
    | TokenHead
    | TokenTail
    | TokenWhile
    | TokenDo
    | TokenRead
    | TokenWrite
    | TokenVar String
    deriving (Show, Eq)
}
