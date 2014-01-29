{
module Parser where

import Lexer
import Syntax
}

%name parse
%tokentype { Token }
%error { parseError }

%token
    ConsInf { TokenConsInf }
    OpenBrc { TokenOpenBrc }
    ClosBrc { TokenClosBrc }
    OpenCur { TokenOpenCur }
    ClosCur { TokenClosCur }
    IsEq    { TokenIsEq    }
    Assign  { TokenAssign  }
    Nil     { TokenNil     }
    SemiCo  { TokenSemiCo  }
    ConsPos { TokenConsPos }
    Head    { TokenHead    }
    Tail    { TokenTail    }
    While   { TokenWhile   }
    Do      { TokenDo      }
    Read    { TokenRead    }
    Write   { TokenWrite   }
    Var     { TokenVar $$  }

%%

PROGRAM :: { Program }
PROGRAM : Read Var SemiCo COMMLIST Write EXPR { Program $2 $4 $6 }

EXPR :: { Expression }
EXPR : ConsPos EXPR EXPR    { Cons $2 $3 } 
     | EXPR ConsInf EXPR    { Cons $1 $3 }
     | Nil                  { Nil        }
     | Var                  { Var $1     }
     | OpenBrc EXPR ClosBrc { $2         }
     | Head EXPR            { Hd $2      }
     | Tail EXPR            { Tl $2      }
     | IsEq EXPR EXPR       { IsEq $2 $3 }

COMMLIST :: { [Command] }
COMMLIST : COMMAND SemiCo COMMLIST { $1 : $3 }
         | {- empty -}             { []      }

COMMAND :: { Command }
COMMAND : While EXPR Do OpenCur COMMLIST ClosCur { While $2 $5  }
        | Var Assign EXPR                        { Assign $1 $3 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}
