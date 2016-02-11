module SugarSyntax
    ( SuProgram (..)
    , SuCommand (..)
    , checkNoRec
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

data SuProgram = SuProgram Pure.Name SuCommand Expression deriving Eq

type Expression = Pure.Expression
type Command = Pure.Command
 
-- Some convenient shorthands for pure constructs
name fp x = Pure.Name (fp, x)
compos    = Pure.Compos
assign    = Pure.Assign
while     = Pure.While
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
    | IfElse Expression SuCommand SuCommand
    | Macro Pure.Name FilePath Expression
    | Switch Expression [(Expression, SuCommand)] SuCommand
    deriving (Show, Eq, Ord)

checkNoRec :: FilePath -> M.Map FilePath SuProgram -> Bool
checkNoRec file = checkNoRecDFS M.empty (S.singleton file)

-- Make sure that there is no recursion in the macro graph by performing a DFS.
-- Programs are the nodes, and each has an edge to another node/program if it
-- makes a macro call to it.
checkNoRecDFS ::
    M.Map FilePath (S.Set FilePath) -> -- Visited nodes and their edges
    S.Set FilePath                  -> -- To visit
    M.Map FilePath SuProgram        -> -- The entire graph
    Bool                               -- Result - true iff no cycles
checkNoRecDFS ingraph tovisit graph =
    if (S.null tovisit) || (M.null graph) then
        True
    else
    let current     = S.findMin tovisit
        curChildren = macroNamesProg (graph M.! current)
        tovisitRest = S.deleteMin tovisit
        newIngraph  = M.insert current curChildren ingraph
        newTovisit  = S.union tovisitRest curChildren
    in
    if (not . S.null) (S.intersection curChildren (M.keysSet newIngraph)) then
        False
    else
        checkNoRecDFS newIngraph newTovisit graph

-- Get the names of all macro calls made within a given SuProgram
macroNamesProg :: SuProgram -> S.Set FilePath
macroNamesProg (SuProgram _ sc _) = macroNames sc

-- Get the names of all macro calls made within a given SuCommand
macroNames :: SuCommand -> S.Set FilePath
macroNames sc = case sc of
    SuCompos c d -> S.union (macroNames c) (macroNames d)
    SuAssign _ _ -> S.empty
    SuWhile  _ c -> macroNames c
    IfElse _ c d -> S.union (macroNames c) (macroNames d)
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
    IfElse gd c1 c2    -> translateConditional gd (desugared c1) (desugared c2)
    Macro x f e        -> case M.lookup f macros of
        Just (SuProgram rd mcom wrt) ->
            compos (Pure.Assign rd e)
                (compos (desugared mcom) (Pure.Assign x wrt))
        Nothing -> error $ "Macro '" ++ f ++ "' not found while desugaring"
    Switch e cases def -> translateSwitch e
        (map (\(e, c) -> (e, desugared c)) cases) (desugared def)

{-- Translate a parsed if-then-else into pure while. The while code below shows
    how these are translated into pure while - stacks are used to ensure that
    these can be nested recursively.

        _NOT_EXP_VAL_STACK__ := cons cons nil nil _NOT_EXP_VAL_STACK__;
        _EXP_VAL_STACK_      := cons E _EXP_VAL_STACK_;
        while hd _EXP_VAL_STACK_ do
            { _EXP_VAL_STACK_      := cons nil tl _EXP_VAL_STACK_
            ; _NOT_EXP_VAL_STACK__ := cons nil tl _NOT_EXP_VAL_STACK__
            ; C1
            }
        while hd _NOT_EXP_VAL_STACK__ do
            { _NOT_EXP_VAL_STACK__ := cons nil tl _NOT_EXP_VAL_STACK__
            ; C2
            }
        _NOT_EXP_VAL_STACK__ := tl _NOT_EXP_VAL_STACK__;
        _EXP_VAL_STACK_      := tl _EXP_VAL_STACK_;

    The variable names used for these stacks will not be accepted by the lexer,
    so they are guaranteed not to interfere with the programmer's choice of
    variable names.
--}
translateConditional :: Expression -> Command -> Command -> Command
translateConditional guard commTrue commFalse =
    compos (compos (compos (compos (compos
        (assign notExpStack (cons (cons nil nil) (var notExpStack)))
        (assign expValStack (cons guard (var expValStack))))
        (while (hd (var expValStack)) (compos (compos
            (assign expValStack (cons nil (tl (var expValStack))))
            (assign notExpStack (cons nil (tl (var notExpStack)))))
            commTrue)))
        (while (hd (var notExpStack)) (compos
            (assign notExpStack (cons nil (tl (var notExpStack))))
            commFalse)))
        (assign notExpStack (tl (var notExpStack))))
        (assign expValStack (tl (var expValStack)))
    where
    expValStack = name "+IMPL+" "+EXP+VAL+STACK+"
    notExpStack = name "+IMPL+" "+NOT+EXP+STACK+"

{-- Translate a switch block - first translate to a conditional and then
    translate the conditional to pure syntax.
--}
translateSwitch :: Expression -> [(Expression, Command)] -> Command -> Command
translateSwitch exp  []                     def = def
translateSwitch expA ((expB, comm) : cases) def =
    translateConditional (iseq expA expB) comm (translateSwitch expA cases def)
