{
module Parser where

import Lexer
import Syntax
}

%name parseProg PROGRAM
%name parseExpr EXPR
%name parseComm COMMAND

%tokentype { Token }
%error { parseError }

%token
    ConsInf { TokenConsInf p    }
    OpenBrc { TokenOpenBrc p    }
    ClosBrc { TokenClosBrc p    }
    OpenCur { TokenOpenCur p    }
    ClosCur { TokenClosCur p    }
    IsEq    { TokenIsEq    p    }
    Assign  { TokenAssign  p    }
    Nil     { TokenNil     p    }
    SemiCo  { TokenSemiCo  p    }
    ConsPre { TokenConsPre p    }
    Head    { TokenHead    p    }
    Tail    { TokenTail    p    }
    While   { TokenWhile   p    }
    Do      { TokenDo      p    }
    Read    { TokenRead    p    }
    Write   { TokenWrite   p    }
    Var     { TokenVar     p $$ }

%%

PROGRAM :: { Program }
PROGRAM : Read Var SemiCo COMMLIST Write EXPR { Program $2 $4 $6 }

EXPR :: { Expression }
EXPR : ConsPre EXPR EXPR    { Cons $2 $3 } 
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
parseError []           = error "Parse error: reached end of file while parsing"
parseError (tok : rest) = error $ concat
    [ "Parse error: "
    , (tokStr tok)
    , " at line "
    , (show (lineNo tok))
    , ", char "
    , (show (charNo tok))
    ]
}
