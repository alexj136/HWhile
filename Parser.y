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

%right IsEq
%right ConsPre
%right ConsInf
%right Head Tail
%left SemiCo

%token
    ConsInf { TokenConsInf p    }
    OpenBrc { TokenOpenBrc p    }
    ClosBrc { TokenClosBrc p    }
    OpenCur { TokenOpenCur p    }
    ClosCur { TokenClosCur p    }
    OpenSqu { TokenOpenSqu p    }
    ClosSqu { TokenClosSqu p    }
    Comma   { TokenComma   p    }
    IsEq    { TokenIsEq    p    }
    Assign  { TokenAssign  p    }
    Nil     { TokenNil     p    }
    SemiCo  { TokenSemiCo  p    }
    ConsPre { TokenConsPre p    }
    Head    { TokenHead    p    }
    Tail    { TokenTail    p    }
    While   { TokenWhile   p    }
    Do      { TokenDo      p    }
    If      { TokenIf      p    }
    Then    { TokenThen    p    }
    Else    { TokenElse    p    }
    Read    { TokenRead    p    }
    Write   { TokenWrite   p    }
    Var     { TokenVar     p $$ }
    Int     { TokenInt     p $$ }

%%

PROGRAM :: { Program }
PROGRAM : Read Var SemiCo COMMAND SemiCo Write EXPR { Program $2 $4 $7 }

EXPR :: { Expression }
EXPR : ConsPre EXPR EXPR    { Cons $2 $3         } 
     | EXPR ConsInf EXPR    { Cons $1 $3         }
     | Nil                  { Nil                }
     | Var                  { Var $1             }
     | Int                  { mkInt $1           }
     | OpenBrc EXPR ClosBrc { $2                 }
     | Head EXPR            { Hd $2              }
     | Tail EXPR            { Tl $2              }
     | IsEq EXPR EXPR       { IsEq $2 $3         }
     | EXPLIST              { listToWhileList $1 }

EXPLIST :: { [Expression] }
EXPLIST : OpenSqu ClosSqu       { []      }
        | OpenSqu EXPR RESTLIST { $2 : $3 }

RESTLIST :: { [Expression] }
RESTLIST : Comma EXPR RESTLIST { $2 : $3 }
         | ClosSqu             { []      }

COMMAND :: { Command }
COMMAND : COMMAND SemiCo COMMAND                { Compos $1 $3 }
        | Var Assign EXPR                       { Assign $1 $3 }
        | While EXPR Do OpenCur COMMAND ClosCur { While  $2 $5 }
        | If EXPR Then OpenCur COMMAND ClosCur Else OpenCur COMMAND ClosCur { transCond $2 $5 $9 }

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

-- Takes a string representation of an integer and converts it into the
-- equivalent Expression representation
mkInt :: String -> Expression
mkInt s = intToExp (read s) Nil

-- Makes an Expression from an Int, using accumulating parameter style
intToExp :: Int -> Expression -> Expression
intToExp 0 acc = acc
intToExp n acc = intToExp (n - 1) (Cons Nil acc)

-- Convert a parsed list of Expressions into an actual while list
listToWhileList :: [Expression] -> Expression
listToWhileList (h:t) = Cons h (listToWhileList t)
listToWhileList []    = Nil

-- Translate a parsed if-then-else into pure while
transCond :: Expression -> Command -> Command -> Command
transCond gd c1 c2 =
    Compos (Compos (Compos (Compos (Compos
        (Assign "+NOT+EXP+STACK+" (Cons (Cons Nil Nil) (Var "+NOT+EXP+STACK+")))
        (Assign "+EXP+VAL+STACK+" (Cons gd (Var "+EXP+VAL+STACK+"))))
        (While (Hd (Var "+EXP+VAL+STACK+")) (Compos (Compos
            (Assign "+EXP+VAL+STACK+" (Cons Nil (Tl (Var "+EXP+VAL+STACK+"))))
            (Assign "+NOT+EXP+STACK+" (Cons Nil (Tl (Var "+NOT+EXP+STACK+")))))
            c1)))
        (While (Hd (Var "+NOT+EXP+STACK+")) (Compos
            (Assign "+NOT+EXP+STACK+" (Cons Nil (Tl (Var "+NOT+EXP+STACK+"))))
            c2)))
        (Assign "+NOT+EXP+STACK+" (Tl (Var "+NOT+EXP+STACK+"))))
        (Assign "+EXP+VAL+STACK+" (Tl (Var "+EXP+VAL+STACK+")))
}
