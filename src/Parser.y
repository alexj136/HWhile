{
module Parser where

import Lexer
import PureSyntax
import SugarSyntax
}

%name parseProg PROGRAM
%name parseExpr EXPR
%name parseComm COMMAND
%name parseVal  VAL

%tokentype { Token }
%error { parseError }

%right IsEq
%right ConsPre
%right ConsInf
%right Hd Tl
%left SemiCo

%token
    Dot       { Token ( _, TkDot         , _ ) }
    OpenBrc   { Token ( _, TkOpenBrc     , _ ) }
    ClosBrc   { Token ( _, TkClosBrc     , _ ) }
    OpenCur   { Token ( _, TkOpenCur     , _ ) }
    ClosCur   { Token ( _, TkClosCur     , _ ) }
    OpenSqu   { Token ( _, TkOpenSqu     , _ ) }
    ClosSqu   { Token ( _, TkClosSqu     , _ ) }
    Comma     { Token ( _, TkComma       , _ ) }
    IsEq      { Token ( _, TkIsEq        , _ ) }
    Assign    { Token ( _, TkAssign      , _ ) }
    Nil       { Token ( _, TkNil         , _ ) }
    SemiCo    { Token ( _, TkSemiCo      , _ ) }
    Cons      { Token ( _, TkCons        , _ ) }
    Hd        { Token ( _, TkHd          , _ ) }
    Tl        { Token ( _, TkTl          , _ ) }
    While     { Token ( _, TkWhile       , _ ) }
    Switch    { Token ( _, TkSwitch      , _ ) }
    Case      { Token ( _, TkCase        , _ ) }
    Default   { Token ( _, TkDefault     , _ ) }
    Colon     { Token ( _, TkColon       , _ ) }
    If        { Token ( _, TkIf          , _ ) }
    Else      { Token ( _, TkElse        , _ ) }
    Read      { Token ( _, TkRead        , _ ) }
    Write     { Token ( _, TkWrite       , _ ) }
    AtAsgn    { Token ( _, TkAtomAsgn    , _ ) }
    AtDoAsgn  { Token ( _, TkAtomDoAsgn  , _ ) }
    AtWhile   { Token ( _, TkAtomWhile   , _ ) }
    AtDoWhile { Token ( _, TkAtomDoWhile , _ ) }
    AtIf      { Token ( _, TkAtomIf      , _ ) }
    AtDoIf    { Token ( _, TkAtomDoIf    , _ ) }
    AtVar     { Token ( _, TkAtomVar     , _ ) }
    AtQuote   { Token ( _, TkAtomQuote   , _ ) }
    AtHd      { Token ( _, TkAtomHd      , _ ) }
    AtDoHd    { Token ( _, TkAtomDoHd    , _ ) }
    AtTl      { Token ( _, TkAtomTl      , _ ) }
    AtDoTl    { Token ( _, TkAtomDoTl    , _ ) }
    AtCons    { Token ( _, TkAtomCons    , _ ) }
    AtDoCons  { Token ( _, TkAtomDoCons  , _ ) }
    Var       { Token ( _, ITkVar   _    , _ ) }
    Int       { Token ( _, ITkInt   $$   , _ ) }
    Macro     { Token ( _, ITkMacro $$   , _ ) }

%%

PROGRAM :: { SuProgram }
PROGRAM : Read VAR BLOCK Write VAR { SuProgram $2 $3 $5 }

VAR :: { Name }
VAR : Var { Name (pathOf $1, varName $1) }

EXPR :: { Expression }
EXPR : Cons EXPR EXPR       { Cons $2 $3         }
     | EXPR Dot EXPR        { Cons $1 $3         }
     | Nil                  { Nil                }
     | VAR                  { Var $1             }
     | Int                  { intToExp $1 Nil    }
     | OpenBrc EXPR ClosBrc { $2                 }
     | Hd EXPR              { Hd $2              }
     | Tl EXPR              { Tl $2              }
     | EXPR IsEq EXPR       { IsEq $1 $3         }
     | EXPLIST              { listToWhileList $1 }
     | ATOM                 { $1                 }

ATOM :: { Expression }
ATOM : AtAsgn               { intToExp  2 Nil    }
     | AtDoAsgn             { intToExp  3 Nil    }
     | AtWhile              { intToExp  5 Nil    }
     | AtDoWhile            { intToExp  7 Nil    }
     | AtIf                 { intToExp 11 Nil    }
     | AtDoIf               { intToExp 13 Nil    }
     | AtVar                { intToExp 17 Nil    }
     | AtQuote              { intToExp 19 Nil    }
     | AtHd                 { intToExp 23 Nil    }
     | AtDoHd               { intToExp 29 Nil    }
     | AtTl                 { intToExp 31 Nil    }
     | AtDoTl               { intToExp 37 Nil    }
     | AtCons               { intToExp 41 Nil    }
     | AtDoCons             { intToExp 43 Nil    }

EXPLIST :: { [Expression] }
EXPLIST : OpenSqu ClosSqu          { []      }
        | OpenSqu EXPR RESTEXPLIST { $2 : $3 }

RESTEXPLIST :: { [Expression] }
RESTEXPLIST : Comma EXPR RESTEXPLIST { $2 : $3 }
            | ClosSqu                { []      }

COMMAND :: { SuCommand }
COMMAND : COMMAND SemiCo COMMAND         { SuCompos $1 $3                }
        | VAR Assign EXPR                { SuAssign $1 $3                }
        | VAR Assign Macro EXPR          { Macro $1 $3 $4                }
        | While EXPR BLOCK               { SuWhile  $2 $3                }
        | If EXPR BLOCK Else BLOCK       { SuIfElse $2 $3 $5             }
        | If EXPR BLOCK                  { SuIfElse $2 $3 skip           }
        | Switch EXPR OpenCur SWITCHCONT { Switch   $2 (fst $4) (snd $4) }

BLOCK :: { SuCommand }
BLOCK : OpenCur COMMAND ClosCur { $2   }
      | OpenCur         ClosCur { skip }

SWITCHCONT :: { ([(Expression, SuCommand)], SuCommand) }
SWITCHCONT : Case EXPR Colon COMMAND SWITCHCONT { (($2, $4) : fst $5, snd $5) }
           | ClosCur                            { ([]               , skip  ) }
           | Default Colon COMMAND ClosCur      { ([]               , $3    ) }

-- For command line input
VAL :: { Expression }
VAL : Nil                 { Nil                }
    | VAL Dot VAL         { Cons $1 $3         }
    | Int                 { intToExp $1 Nil    }
    | OpenBrc VAL ClosBrc { $2                 }
    | VALLIST             { listToWhileList $1 }
    | ATOM                { $1                 }

VALLIST :: { [Expression] }
VALLIST : OpenSqu ClosSqu         { []      }
        | OpenSqu VAL RESTVALLIST { $2 : $3 }

RESTVALLIST :: { [Expression] }
RESTVALLIST : Comma VAL RESTVALLIST { $2 : $3 }
            | ClosSqu               { []      }
{
parseError :: [Token] -> a
parseError []           = error "Parse error: reached end of file while parsing"
parseError (tok : rest) = error $ concat
    [ "Parse error: "
    , (prettyPrintToken tok)
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
