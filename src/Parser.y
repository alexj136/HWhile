{
module Parser where

import Lexer
import PureSyntax
import SugarSyntax
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
    ConsInf { SimpleToken p TkConsInf }
    OpenBrc { SimpleToken p TkOpenBrc }
    ClosBrc { SimpleToken p TkClosBrc }
    OpenCur { SimpleToken p TkOpenCur }
    ClosCur { SimpleToken p TkClosCur }
    OpenSqu { SimpleToken p TkOpenSqu }
    ClosSqu { SimpleToken p TkClosSqu }
    Comma   { SimpleToken p TkComma   }
    IsEq    { SimpleToken p TkIsEq    }
    Assign  { SimpleToken p TkAssign  }
    Nil     { SimpleToken p TkNil     }
    SemiCo  { SimpleToken p TkSemiCo  }
    ConsPre { SimpleToken p TkConsPre }
    Head    { SimpleToken p TkHead    }
    Tail    { SimpleToken p TkTail    }
    While   { SimpleToken p TkWhile   }
    Switch  { SimpleToken p TkSwitch  }
    Case    { SimpleToken p TkCase    }
    Default { SimpleToken p TkDefault }
    Colon   { SimpleToken p TkColon   }
    If      { SimpleToken p TkIf      }
    Else    { SimpleToken p TkElse    }
    Read    { SimpleToken p TkRead    }
    Write   { SimpleToken p TkWrite   }
    Var     { TokenVar    p $$        }
    Int     { TokenInt    p $$        }

%%

PROGRAM :: { SuProgram }
PROGRAM : Read Var SemiCo COMMAND SemiCo Write EXPR {
        SuProgram (Name $2) $4 $7 }

EXPR :: { Expression }
EXPR : ConsPre EXPR EXPR    { Cons $2 $3         }
     | EXPR ConsInf EXPR    { Cons $1 $3         }
     | Nil                  { Nil                }
     | Var                  { Var (Name $1)      }
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

COMMAND :: { SuCommand }
COMMAND : COMMAND SemiCo COMMAND         { SuCompos $1 $3                }
        | Var Assign EXPR                { SuAssign (Name $1) $3         }
        | While EXPR BLOCK               { SuWhile  $2 $3                }
        | If EXPR BLOCK Else BLOCK       { IfElse   $2 $3 $5             }
        | If EXPR BLOCK                  { IfElse   $2 $3 skip           }
        | Switch EXPR OpenCur SWITCHCONT { Switch   $2 (fst $4) (snd $4) }

BLOCK :: { SuCommand }
BLOCK : OpenCur COMMAND ClosCur { $2 }

SWITCHCONT :: { ([(Expression, SuCommand)], SuCommand) }
SWITCHCONT : Case EXPR Colon COMMAND SWITCHCONT { (($2, $4) : fst $5, snd $5) }
           | ClosCur                            { ([]               , skip  ) }
           | Default Colon COMMAND ClosCur      { ([]               , $3    ) }

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

-- A command that does nothing (simplifies ifs without elses and switches
-- without defaults
skip :: SuCommand
skip = SuAssign (Name "+DEAD+") (Var (Name "+DEAD+"))
}
