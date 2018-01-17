module SugarSyntax where

{- This module defines a higher-level version of the syntax for commands,
 - including conditional, macro and switch commands. This module is included for
 - easier parsing and translation to pure syntax.
 -}

import qualified Data.Set as S
import PureSyntax

data SuProgram = SuProgram Name Name SuBlock Name deriving Eq
 
type SuBlock = [SuCommand]

newtype Info = Info { getInfo :: (FilePath, Int) } deriving (Show, Eq, Ord)

fileInfo :: Info -> FilePath
fileInfo = fst . getInfo

lineInfo :: Info -> Int
lineInfo = snd. getInfo

-- The sugared command syntax - has conditionals, macros and switches in
-- addition to the pure syntax commands.
data SuCommand
    = SuAssign Info Name Expression
    | SuWhile  Info Expression SuBlock
    | SuIfElse Info Expression SuBlock SuBlock
    | Macro    Info Name FilePath Expression
    | Switch   Info Expression [(Expression, SuBlock)] SuBlock
    deriving (Show, Eq, Ord)

namesSuProg :: SuProgram -> S.Set Name
namesSuProg (SuProgram n r b w) = foldr S.insert (namesSuBlock b) [n, r, w]

namesSuBlock :: SuBlock -> S.Set Name
namesSuBlock = S.unions . map namesSuComm

namesSuComm :: SuCommand -> S.Set Name
namesSuComm comm = case comm of
    SuAssign _ n e     -> S.insert n (namesExpr e)
    SuWhile  _ e b     -> S.union (namesExpr e) (namesSuBlock b)
    SuIfElse _ e bt bf -> S.unions [namesExpr e, namesSuBlock bt, namesSuBlock bf]
    Macro    _ n _  e  -> S.insert n (namesExpr e)
    Switch   _ e eb b  -> S.unions
        [ namesExpr e
        , S.unions (map (\(e, b) -> S.union (namesExpr e) (namesSuBlock b)) eb)
        , namesSuBlock b
        ]
