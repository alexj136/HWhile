module InterSyntax where

{- This module defines an intermediate-level version of the syntax for commands,
 - including conditional and switch commands, but no macros. This module is
 - included for interactive execution, and as an intermediate step before
 - translation to pure while syntax.
 -}

import qualified Data.Set as S
import PureSyntax
import SugarSyntax (Info)

data InProgram = InProgram
    { inProgName :: Name
    , inReadVar  :: Name
    , inBlock    :: InBlock
    , inWriteVar :: Name
    } deriving (Eq, Ord)
 
type InBlock = [InCommand]

-- The sugared command syntax - has conditionals, macros and switches in
-- addition to the pure syntax commands.
data InCommand
    = InAssign Info Name Expression
    | InWhile  Info Expression InBlock
    | InIfElse Info Expression InBlock InBlock
    | InSwitch Info Expression [(Expression, InBlock)] InBlock
    deriving (Show, Eq, Ord)

info :: InCommand -> Info
info comm = case comm of
    InAssign i _ _   -> i
    InWhile  i _ _   -> i
    InIfElse i _ _ _ -> i
    InSwitch i _ _ _ -> i

namesInProg :: InProgram -> S.Set Name
namesInProg (InProgram n r b w) = foldr S.insert (namesInBlock b) [n, r, w]

namesInBlock :: InBlock -> S.Set Name
namesInBlock = S.unions . map namesInComm

namesInComm :: InCommand -> S.Set Name
namesInComm comm = case comm of
    InAssign _ n e     -> S.insert n (namesExpr e)
    InWhile  _ e b     -> S.union (namesExpr e) (namesInBlock b)
    InIfElse _ e bt bf -> S.unions [namesExpr e, namesInBlock bt, namesInBlock bf]
    InSwitch _ e eb b  -> S.unions
        [ namesExpr e
        , S.unions (map (\(e, b) -> S.union (namesExpr e) (namesInBlock b)) eb)
        , namesInBlock b
        ]
