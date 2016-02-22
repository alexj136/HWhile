module SugarSyntax
    ( SuProgram (..)
    , SuCommand (..)
    , SuBlock
    , desugarProg
    , macroNamesProg
    ) where

{- This module defines a higher-level version of the syntax for commands,
 - including conditional, macro and switch commands. This module is included for
 - easier parsing and translation to pure syntax.
 -}

import qualified Data.Set   as S
import qualified Data.Map   as M
import qualified PureSyntax as Pure

data SuProgram = SuProgram Pure.Name SuBlock Pure.Name deriving Eq

type Expression = Pure.Expression
type Command = Pure.Command
type Block = Pure.Block
 
-- Some convenient shorthands for pure constructs
name fp x = Pure.Name (fp, x)
assign    = Pure.Assign
while     = Pure.While
ifelse    = Pure.IfElse
cons      = Pure.Cons
var       = Pure.Var
hd        = Pure.Hd
tl        = Pure.Tl
nil       = Pure.Lit Pure.ENil
iseq      = Pure.IsEq

type SuBlock = [SuCommand]

-- The sugared command syntax - has conditionals, macros and switches in
-- addition to the pure syntax commands.
data SuCommand
    = SuAssign Pure.Name Expression
    | SuWhile Expression SuBlock
    | SuIfElse Expression SuBlock SuBlock
    | Macro Pure.Name FilePath Expression
    | Switch Expression [(Expression, SuBlock)] SuBlock
    deriving (Show, Eq, Ord)

-- Get the names of all macro calls made within a given SuProgram
macroNamesProg :: SuProgram -> S.Set FilePath
macroNamesProg (SuProgram _ b _) = macroNamesBlock b

macroNamesBlock :: SuBlock -> S.Set FilePath
macroNamesBlock = S.unions . (map macroNames)

-- Get the names of all macro calls made within a given SuCommand
macroNames :: SuCommand -> S.Set FilePath
macroNames sc = case sc of
    SuAssign _ _   -> S.empty
    SuWhile  _ b   -> macroNamesBlock b
    SuIfElse _ a b -> S.union (macroNamesBlock a) (macroNamesBlock b)
    Macro  _ f _   -> S.singleton f
    Switch _ l b   -> S.union (macroNamesBlock b)
        (S.unions ((map (macroNamesBlock . snd)) l))

-- Desugar a program, that is, convert it to pure while syntax
desugarProg :: M.Map FilePath SuProgram -> SuProgram -> Pure.Program
desugarProg macros (SuProgram n blk e) =
    Pure.Program n (desugarBlock macros blk) e

desugarBlock :: M.Map FilePath SuProgram -> SuBlock -> Block
desugarBlock macros = concat . (map (desugar macros))

-- Desugar a command
desugar :: M.Map FilePath SuProgram -> SuCommand -> Block
desugar macros suComm = let desugared = desugarBlock macros in case suComm of
    SuAssign x exp     -> [ Pure.Assign x exp                       ]
    SuWhile  gd c      -> [ while gd (desugared c)                  ]
    SuIfElse gd c1 c2  -> [ ifelse gd (desugared c1) (desugared c2) ]
    Macro x f e        -> case M.lookup f macros of
        Just (SuProgram rd mblk wrt) -> [ Pure.Assign rd e ]
            ++ desugared mblk ++ [ Pure.Assign x (var wrt) ]
        Nothing -> error $ "Macro '" ++ f ++ "' not found while desugaring"
    Switch e cases def -> translateSwitch e
        (map (\(e, b) -> (e, desugared b)) cases) (desugared def)

{-- Translate a switch block - first translate to a conditional and then
    translate the conditional to pure syntax.
--}
translateSwitch :: Expression -> [(Expression, Block)] -> Block -> Block
translateSwitch exp  []                    def = def
translateSwitch expA ((expB, blk) : cases) def =
    [ ifelse (iseq expA expB) blk (translateSwitch expA cases def) ]
