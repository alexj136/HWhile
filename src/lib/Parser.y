{
module Parser where

import Lexer
import PureSyntax
import SugarSyntax
}

%name parseProg PROGRAM
%name parseExpr EXPR
%name parseComm COMMAND
%name parseLVal LVAL

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
    OpenAng   { Token ( _, TkOpenAng     , _ ) }
    ClosAng   { Token ( _, TkClosAng     , _ ) }
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
    True      { Token ( _, TkTrue        , _ ) }
    False     { Token ( _, TkFalse       , _ ) }
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
    Var       { Token ( _, ITkVar _      , _ ) }
    Int       { Token ( _, ITkInt $$     , _ ) }

%%

PROGRAM :: { SuProgram }
PROGRAM : VAR Read VAR BLOCK Write VAR {
              if namePath $1 == nameName $1 then SuProgram $3 $4 $6
              else error $ "Program name must match the file prefix." }

VAR :: { Name }
VAR : Var { Name (tkPath $1, tkVarName $1) }

EXPR :: { Expression }
EXPR : Cons EXPR EXPR       { Cons $2 $3            }
     | VAR                  { Var $1                }
     | OpenBrc EXPR ClosBrc { $2                    }
     | Hd EXPR              { Hd $2                 }
     | Tl EXPR              { Tl $2                 }
     | EXPR IsEq EXPR       { IsEq $1 $3            }
     | EXPLIST              { expListToWhileList $1 }
     | VAL                  { Lit $1                }

ATOM :: { ETree }
ATOM : AtAsgn    { intToExp  2 ENil }
     | AtDoAsgn  { intToExp  3 ENil }
     | AtWhile   { intToExp  5 ENil }
     | AtDoWhile { intToExp  7 ENil }
     | AtIf      { intToExp 11 ENil }
     | AtDoIf    { intToExp 13 ENil }
     | AtVar     { intToExp 17 ENil }
     | AtQuote   { intToExp 19 ENil }
     | AtHd      { intToExp 23 ENil }
     | AtDoHd    { intToExp 29 ENil }
     | AtTl      { intToExp 31 ENil }
     | AtDoTl    { intToExp 37 ENil }
     | AtCons    { intToExp 41 ENil }
     | AtDoCons  { intToExp 43 ENil }

EXPLIST :: { [Expression] }
EXPLIST : OpenSqu ClosSqu          { []      }
        | OpenSqu EXPR RESTEXPLIST { $2 : $3 }

RESTEXPLIST :: { [Expression] }
RESTEXPLIST : Comma EXPR RESTEXPLIST { $2 : $3 }
            | ClosSqu                { []      }

COMMAND :: { SuCommand }
COMMAND : VAR Assign EXPR                     { SuAssign $1 $3              }
        | VAR Assign OpenAng VAR ClosAng EXPR { Macro $1 (nameName $4) $6   }
        | While EXPR BLOCK                    { SuWhile $2 $3               }
        | If EXPR BLOCK Else BLOCK            { SuIfElse $2 $3 $5           }
        | If EXPR BLOCK                       { SuIfElse $2 $3 []           }
        | Switch EXPR OpenCur SWITCHCONT      { Switch $2 (fst $4) (snd $4) }

BLOCK :: { SuBlock }
BLOCK : OpenCur      ClosCur { [] }
      | OpenCur CMDS ClosCur { $2 }

CMDS :: { SuBlock }
CMDS : COMMAND SemiCo CMDS { $1 : $3 }
     | COMMAND             { [$1]    }

SWITCHCONT :: { ([(Expression, SuBlock)], SuBlock) }
SWITCHCONT : Case EXPR Colon CMDS SWITCHCONT { (($2, $4) : fst $5, snd $5) }
           | ClosCur                         { ([]               , []    ) }
           | Default Colon CMDS ClosCur      { ([]               , $3    ) }

VAL :: { ETree }
VAL : Nil                         { ENil             }
    | OpenAng VAL Dot VAL ClosAng { ECons $2 $4      }
    | Int                         { intToExp $1 ENil }
    | True                        { intToExp 1 ENil  }
    | False                       { intToExp 0 ENil  }
    | ATOM                        { $1               }

-- LVALs are VALs, but also with lists allowed. They are only used for command
-- line input, as adding lists to general VALs that occur within EXPs casues
-- ambiguity.
LVAL :: { ETree }
LVAL : Nil                           { ENil                  }
     | OpenAng LVAL Dot LVAL ClosAng { ECons $2 $4           }
     | Int                           { intToExp $1 ENil      }
     | True                          { intToExp 1 ENil       }
     | False                         { intToExp 0 ENil       }
     | LVALLIST                      { litListToWhileList $1 }
     | ATOM                          { $1                    }

LVALLIST :: { [ETree] }
LVALLIST : OpenSqu ClosSqu           { []      }
         | OpenSqu LVAL RESTLVALLIST { $2 : $3 }

RESTLVALLIST :: { [ETree] }
RESTLVALLIST : Comma LVAL RESTLVALLIST { $2 : $3 }
             | ClosSqu                 { []      }
{
parseError :: [Token] -> a
parseError []           = error "Parse error: reached end of file while parsing"
parseError (tok : rest) = error $ concat
    [ "Parse error: "
    , (prettyPrintToken tok)
    , " at line "
    , (show (tkLineNo tok))
    , ", char "
    , (show (tkCharNo tok))
    ]

-- Makes an Expression from an Int, using accumulating parameter style
intToExp :: Int -> ETree -> ETree
intToExp 0 acc = acc
intToExp n acc = intToExp (n - 1) (ECons ENil acc)

-- Convert a parsed list of Expressions into an actual while list
expListToWhileList :: [Expression] -> Expression
expListToWhileList (h:t) = Cons h (expListToWhileList t)
expListToWhileList []    = Lit ENil

-- Convert a parsed list of ETrees into an actual while list
litListToWhileList :: [ETree] -> ETree
litListToWhileList (h:t) = ECons h (litListToWhileList t)
litListToWhileList []    = ENil
}
