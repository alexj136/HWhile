module SugarSyntax
    ( SuProgram (..)
    , SuCommand (..)
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

data SuProgram = SuProgram Pure.Name SuCommand Pure.Name deriving Eq

type Expression = Pure.Expression
type Command = Pure.Command
 
-- Some convenient shorthands for pure constructs
name fp x = Pure.Name (fp, x)
compos    = Pure.Compos
assign    = Pure.Assign
while     = Pure.While
ifelse    = Pure.IfElse
cons      = Pure.Cons
var       = Pure.Var
hd        = Pure.Hd
tl        = Pure.Tl
nil       = Pure.Nil
iseq      = Pure.IsEq

-- The sugared command syntax - has conditionals, macros and switches in
-- addition to the pure syntax commands.
data SuCommand
    = SuCompos SuCommand SuCommand
    | SuAssign Pure.Name Expression
    | SuWhile Expression SuCommand
    | SuIfElse Expression SuCommand SuCommand
    | Macro Pure.Name FilePath Expression
    | Switch Expression [(Expression, SuCommand)] SuCommand
    deriving (Show, Eq, Ord)

-- Get the names of all macro calls made within a given SuProgram
macroNamesProg :: SuProgram -> S.Set FilePath
macroNamesProg (SuProgram _ sc _) = macroNames sc

-- Get the names of all macro calls made within a given SuCommand
macroNames :: SuCommand -> S.Set FilePath
macroNames sc = case sc of
    SuCompos c d -> S.union (macroNames c) (macroNames d)
    SuAssign _ _ -> S.empty
    SuWhile  _ c -> macroNames c
    SuIfElse _ c d -> S.union (macroNames c) (macroNames d)
    Macro  _ f _ -> S.singleton f
    Switch _ l c -> S.union (macroNames c)
        (S.unions ((map (macroNames . snd)) l))

-- Desugar a program, that is, convert it to pure while syntax
desugarProg :: M.Map FilePath SuProgram -> SuProgram -> Pure.Program
desugarProg macros (SuProgram n sc e) = Pure.Program n (desugar macros sc) e

-- Desugar a command
desugar :: M.Map FilePath SuProgram -> SuCommand -> Command
desugar macros suComm = let desugared = desugar macros in case suComm of
    SuCompos c1 c2     -> compos (desugared c1) (desugared c2)
    SuAssign x exp     -> Pure.Assign x exp
    SuWhile  gd c      -> while gd (desugared c)
    SuIfElse gd c1 c2  -> ifelse gd (desugared c1) (desugared c2)
    Macro x f e        -> case M.lookup f macros of
        Just (SuProgram rd mcom wrt) ->
            compos (Pure.Assign rd e)
                (compos (desugared mcom) (Pure.Assign x (var wrt)))
        Nothing -> error $ "Macro '" ++ f ++ "' not found while desugaring"
    Switch e cases def -> translateSwitch e
        (map (\(e, c) -> (e, desugared c)) cases) (desugared def)

{-- Translate a switch block - first translate to a conditional and then
    translate the conditional to pure syntax.
--}
translateSwitch :: Expression -> [(Expression, Command)] -> Command -> Command
translateSwitch exp  []                     def = def
translateSwitch expA ((expB, comm) : cases) def =
    ifelse (iseq expA expB) comm (translateSwitch expA cases def)
