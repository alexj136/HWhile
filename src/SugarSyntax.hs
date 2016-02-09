module SugarSyntax
    ( SuProgram (..)
    , SuCommand (..)
    , desugar
    , desugarProg
    ) where

{- This module defines a higher-level version of the syntax for commands,
 - including with conditional and pattern-matching commands. This module is
 - included for easier parsing and translation to pure syntax.
 -}

import qualified Data.Set   as S
import qualified PureSyntax as Pure

data SuProgram = SuProgram Pure.Name SuCommand Expression deriving Eq

desugarProg :: SuProgram -> Pure.Program
desugarProg (SuProgram n sc e) = Pure.Program n (desugar sc) e

type Expression = Pure.Expression
type Command = Pure.Command
 
compos   = Pure.Compos
assign n = Pure.Assign (Pure.Name n)
while    = Pure.While
cons     = Pure.Cons
var      = Pure.Var . Pure.Name
hd       = Pure.Hd
tl       = Pure.Tl
nil      = Pure.Nil
iseq     = Pure.IsEq

data SuCommand
    = SuCompos SuCommand SuCommand
    | SuAssign Pure.Name Expression
    | SuWhile Expression SuCommand
    | IfElse Expression SuCommand SuCommand
    | Macro FilePath Expression
    | Switch Expression [(Expression, SuCommand)] SuCommand
    deriving (Eq, Ord)

macroNamesProg :: SuProgram -> S.Set FilePath
macroNamesProg (SuProgram _ sc _) = macroNames sc
    where
    macroNames :: SuCommand -> S.Set FilePath
    macroNames sc = case sc of
        SuCompos c d -> S.union (macroNames c) (macroNames d)
        SuWhile  _ c -> macroNames c
        IfElse _ c d -> S.union (macroNames c) (macroNames d)
        Macro    f _ -> S.singleton f
        Switch _ l c ->
            S.union (macroNames c) (S.unions ((map (macroNames . snd)) l))

-- Desugar a command, that is, convert it to pure while syntax
desugar :: SuCommand -> Command
desugar suComm = case suComm of
    SuCompos c1 c2        -> compos (desugar c1) (desugar c2)
    SuAssign x exp        -> Pure.Assign x exp
    SuWhile  gd c         -> while gd (desugar c)
    IfElse gd c1 c2       -> translateConditional gd (desugar c1) (desugar c2)
    Switch e cases def ->
        translateSwitch e (map (\(e, c) -> (e, desugar c)) cases) (desugar def)

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
        (assign "+NOT+EXP+STACK+" (cons (cons nil nil) (var "+NOT+EXP+STACK+")))
        (assign "+EXP+VAL+STACK+" (cons guard (var "+EXP+VAL+STACK+"))))
        (while (hd (var "+EXP+VAL+STACK+")) (compos (compos
            (assign "+EXP+VAL+STACK+" (cons nil (tl (var "+EXP+VAL+STACK+"))))
            (assign "+NOT+EXP+STACK+" (cons nil (tl (var "+NOT+EXP+STACK+")))))
            commTrue)))
        (while (hd (var "+NOT+EXP+STACK+")) (compos
            (assign "+NOT+EXP+STACK+" (cons nil (tl (var "+NOT+EXP+STACK+"))))
            commFalse)))
        (assign "+NOT+EXP+STACK+" (tl (var "+NOT+EXP+STACK+"))))
        (assign "+EXP+VAL+STACK+" (tl (var "+EXP+VAL+STACK+")))

{-- Translate a switch block - first translate to a conditional and then
    translate the conditional to pure syntax.
--}
translateSwitch :: Expression -> [(Expression, Command)] -> Command -> Command
translateSwitch exp  []                     def = def
translateSwitch expA ((expB, comm) : cases) def =
    translateConditional (iseq expA expB) comm (translateSwitch expA cases def)
