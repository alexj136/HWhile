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
%right Hd Tl
%left SemiCo

%token
    Dot     { Token ( _, TkDot       , _ ) }
    OpenBrc { Token ( _, TkOpenBrc   , _ ) }
    ClosBrc { Token ( _, TkClosBrc   , _ ) }
    OpenCur { Token ( _, TkOpenCur   , _ ) }
    ClosCur { Token ( _, TkClosCur   , _ ) }
    OpenSqu { Token ( _, TkOpenSqu   , _ ) }
    ClosSqu { Token ( _, TkClosSqu   , _ ) }
    Comma   { Token ( _, TkComma     , _ ) }
    IsEq    { Token ( _, TkIsEq      , _ ) }
    Assign  { Token ( _, TkAssign    , _ ) }
    Nil     { Token ( _, TkNil       , _ ) }
    SemiCo  { Token ( _, TkSemiCo    , _ ) }
    Cons    { Token ( _, TkCons      , _ ) }
    Hd      { Token ( _, TkHd        , _ ) }
    Tl      { Token ( _, TkTl        , _ ) }
    While   { Token ( _, TkWhile     , _ ) }
    Switch  { Token ( _, TkSwitch    , _ ) }
    Case    { Token ( _, TkCase      , _ ) }
    Default { Token ( _, TkDefault   , _ ) }
    Colon   { Token ( _, TkColon     , _ ) }
    If      { Token ( _, TkIf        , _ ) }
    Else    { Token ( _, TkElse      , _ ) }
    Read    { Token ( _, TkRead      , _ ) }
    Write   { Token ( _, TkWrite     , _ ) }
    Var     { Token ( _, ITkVar   _  , _ ) }
    Int     { Token ( _, ITkInt   $$ , _ ) }
    Macro   { Token ( _, ITkMacro $$ , _ ) }

%%

PROGRAM :: { SuProgram }
PROGRAM : Read Var BLOCK Write EXPR {
              SuProgram (Name (pathOf $2, varName $2)) $3 $5 }

EXPR :: { Expression }
EXPR : Cons EXPR EXPR       { Cons $2 $3                         }
     | EXPR Dot EXPR        { Cons $1 $3                         }
     | Nil                  { Nil                                }
     | Var                  { Var (Name (pathOf $1, varName $1)) }
     | Int                  { intToExp $1 Nil                    }
     | OpenBrc EXPR ClosBrc { $2                                 }
     | Hd EXPR              { Hd $2                              }
     | Tl EXPR              { Tl $2                              }
     | EXPR IsEq EXPR       { IsEq $1 $3                         }
     | EXPLIST              { listToWhileList $1                 }

EXPLIST :: { [Expression] }
EXPLIST : OpenSqu ClosSqu       { []      }
        | OpenSqu EXPR RESTLIST { $2 : $3 }

RESTLIST :: { [Expression] }
RESTLIST : Comma EXPR RESTLIST { $2 : $3 }
         | ClosSqu             { []      }

COMMAND :: { SuCommand }
COMMAND : COMMAND SemiCo COMMAND         { SuCompos $1 $3                      }
        | Var Assign EXPR         { SuAssign (Name (pathOf $1, varName $1)) $3 }
        | Var Assign Macro EXPR   { Macro (Name (pathOf $1, varName $1)) $3 $4 }
        | While EXPR BLOCK               { SuWhile  $2 $3                      }
        | If EXPR BLOCK Else BLOCK       { SuIfElse $2 $3 $5                   }
        | If EXPR BLOCK                  { SuIfElse $2 $3 skip                 }
        | Switch EXPR OpenCur SWITCHCONT { Switch   $2 (fst $4) (snd $4)       }

BLOCK :: { SuCommand }
BLOCK : OpenCur COMMAND ClosCur { $2   }
      | OpenCur         ClosCur { skip }

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
skip = SuAssign (Name ("+IMPL+", "+SKIP+")) (Var (Name ("+IMPL+", "+SKIP+")))
}
