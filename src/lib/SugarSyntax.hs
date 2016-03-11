module SugarSyntax where

{- This module defines a higher-level version of the syntax for commands,
 - including conditional, macro and switch commands. This module is included for
 - easier parsing and translation to pure syntax.
 -}

import qualified Data.Set as S
import PureSyntax

data SuProgram = SuProgram Name Name SuBlock Name deriving Eq
 
type SuBlock = [SuCommand]

-- The sugared command syntax - has conditionals, macros and switches in
-- addition to the pure syntax commands.
data SuCommand
    = SuAssign Name Expression
    | SuWhile Expression SuBlock
    | SuIfElse Expression SuBlock SuBlock
    | Macro Name FilePath Expression
    | Switch Expression [(Expression, SuBlock)] SuBlock
    deriving (Show, Eq, Ord)

namesSuProg :: SuProgram -> S.Set Name
namesSuProg (SuProgram n r b w) = foldr S.insert (namesSuBlock b) [n, r, w]

namesSuBlock :: SuBlock -> S.Set Name
namesSuBlock = S.unions . map namesSuComm

namesSuComm :: SuCommand -> S.Set Name
namesSuComm comm = case comm of
    SuAssign n e     -> S.insert n (namesExpr e)
    SuWhile  e b     -> S.union (namesExpr e) (namesSuBlock b)
    SuIfElse e bt bf -> S.unions [namesExpr e, namesSuBlock bt, namesSuBlock bf]
    Macro    n _  e  -> S.insert n (namesExpr e)
    Switch   e eb b  -> S.unions
        [ namesExpr e
        , S.unions (map (\(e, b) -> S.union (namesExpr e) (namesSuBlock b)) eb)
        , namesSuBlock b
        ]
