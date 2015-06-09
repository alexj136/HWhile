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
    End     { TokenEnd     p    }
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
COMMAND : COMMAND SemiCo COMMAND                { Compos $1 $3       }
        | Var Assign EXPR                       { Assign $1 $3       }
        | While EXPR Do COMMAND End             { While  $2 $4       }
        | If EXPR Then COMMAND Else COMMAND End { transCond $2 $4 $6 }

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

{-- Translate a parsed if-then-else into pure while. The while code below shows
    how these are translated into pure while - stacks are used to ensure that
    these can be nested recursively.

        _NOT_EXP_VAL_STACK__ := cons cons nil nil _NOT_EXP_VAL_STACK__;
        _EXP_VAL_STACK_      := cons E _EXP_VAL_STACK_;
        while hd _EXP_VAL_STACK_ do
            { _EXP_VAL_STACK_      := cons nil tl _EXP_VAL_STACK_
            ; _NOT_EXP_VAL_STACK__ := cons nil tl _NOT_EXP_VAL_STACK__
            ; C1
            }
        while hd _NOT_EXP_VAL_STACK__ do
            { _NOT_EXP_VAL_STACK__ := cons nil tl _NOT_EXP_VAL_STACK__
            ; C2
            }
        _NOT_EXP_VAL_STACK__ := tl _NOT_EXP_VAL_STACK__;
        _EXP_VAL_STACK_      := tl _EXP_VAL_STACK_;

    The variable names used for these stacks will not be accepted by the lexer,
    so they are guaranteed not to interfere with the programmer's choice of
    variable names.
--}
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
