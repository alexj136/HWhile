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
    Dot     { Token ( TkDot       , p ) }
    OpenBrc { Token ( TkOpenBrc   , p ) }
    ClosBrc { Token ( TkClosBrc   , p ) }
    OpenCur { Token ( TkOpenCur   , p ) }
    ClosCur { Token ( TkClosCur   , p ) }
    OpenSqu { Token ( TkOpenSqu   , p ) }
    ClosSqu { Token ( TkClosSqu   , p ) }
    Comma   { Token ( TkComma     , p ) }
    IsEq    { Token ( TkIsEq      , p ) }
    Assign  { Token ( TkAssign    , p ) }
    Nil     { Token ( TkNil       , p ) }
    SemiCo  { Token ( TkSemiCo    , p ) }
    Cons    { Token ( TkCons      , p ) }
    Head    { Token ( TkHead      , p ) }
    Tail    { Token ( TkTail      , p ) }
    While   { Token ( TkWhile     , p ) }
    Switch  { Token ( TkSwitch    , p ) }
    Case    { Token ( TkCase      , p ) }
    Default { Token ( TkDefault   , p ) }
    Colon   { Token ( TkColon     , p ) }
    If      { Token ( TkIf        , p ) }
    Else    { Token ( TkElse      , p ) }
    Read    { Token ( TkRead      , p ) }
    Write   { Token ( TkWrite     , p ) }
    Var     { Token ( ITkGVar  $$ , p ) }
    Int     { Token ( ITkInt   $$ , p ) }
    Macro   { Token ( ITkMacro $$ , p ) }

%%

PROGRAM :: { SuProgram }
PROGRAM : Read Var SemiCo COMMAND SemiCo Write EXPR {
        SuProgram (Name $2) $4 $7 }

EXPR :: { Expression }
EXPR : Cons EXPR EXPR       { Cons $2 $3              }
     | EXPR Dot EXPR        { Cons $1 $3              }
     | Nil                  { Nil                     }
     | Var                  { Var (Name $1)           }
     | Int                  { intToExp $1 Nil         }
     | OpenBrc EXPR ClosBrc { $2                      }
     | Head EXPR            { Hd $2                   }
     | Tail EXPR            { Tl $2                   }
     | IsEq EXPR EXPR       { IsEq $2 $3              }
     | EXPLIST              { listToWhileList $1      }

EXPLIST :: { [Expression] }
EXPLIST : OpenSqu ClosSqu       { []      }
        | OpenSqu EXPR RESTLIST { $2 : $3 }

RESTLIST :: { [Expression] }
RESTLIST : Comma EXPR RESTLIST { $2 : $3 }
         | ClosSqu             { []      }

COMMAND :: { SuCommand }
COMMAND : COMMAND SemiCo COMMAND         { SuCompos $1 $3                }
        | Var Assign EXPR                { SuAssign (Name $1) $3         }
        | Var Assign Macro EXPR          { Macro (Name $1) $3 $4         }
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
    , (show tok)
    , " at line "
    , (show (lineNo tok))
    , ", char "
    , (show (charNo tok))
    ]

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
skip = SuAssign (Name ("+IMPL+", "+DEAD+")) (Var (Name ("+IMPL+", "+DEAD+")))
}
