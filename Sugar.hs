module Sugar where

{- This module defines a higher-level version of the syntax for commands,
 - including with conditional and pattern-matching commands. This module is
 - included for easier parsing and translation to pure syntax.
 -}

import qualified Syntax as Pure

data Command
    = Compos Command Command
    | Assign String Pure.Expression
    | While  Pure.Expression Command
    | IfElse Pure.Expression Command Command
    | PMatch Pure.Expression [(Pure.Expression, Command)]
    deriving Eq

data PMatchExp
    = PMNil

-- Desugar a command, that is, convert it to pure while syntax
desugar :: SuCommand -> Command
desugar suComm = case suComm of
    SuCompos c1 c2    -> Pure.Compos (desugar c1) (desugar c2)
    SuAssign x  exp   -> Pure.Assign x exp
    SuWhile  gd c     -> Pure.While gd (desugar c)
    SuIfElse gd c1 c2 -> translateConditional gd c1 c2

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
translateConditional :: Pure.Expression -> Pure.Command -> Pure.Command -> Pure.Command
translateConditional guard commTrue commFalse =
    Pure.Compos (Pure.Compos (Pure.Compos (Pure.Compos (Pure.Compos
        (Pure.Assign "+NOT+EXP+STACK+" (Cons (Cons Nil Nil) (Var "+NOT+EXP+STACK+")))
        (Pure.Assign "+EXP+VAL+STACK+" (Cons guard (Var "+EXP+VAL+STACK+"))))
        (Pure.While (Hd (Var "+EXP+VAL+STACK+")) (Pure.Compos (Pure.Compos
            (Pure.Assign "+EXP+VAL+STACK+" (Cons Nil (Tl (Var "+EXP+VAL+STACK+"))))
            (Pure.Assign "+NOT+EXP+STACK+" (Cons Nil (Tl (Var "+NOT+EXP+STACK+")))))
            commTrue)))
        (Pure.While (Hd (Var "+NOT+EXP+STACK+")) (Pure.Compos
            (Pure.Assign "+NOT+EXP+STACK+" (Cons Nil (Tl (Var "+NOT+EXP+STACK+"))))
            commFalse)))
        (Pure.Assign "+NOT+EXP+STACK+" (Tl (Var "+NOT+EXP+STACK+"))))
        (Pure.Assign "+EXP+VAL+STACK+" (Tl (Var "+EXP+VAL+STACK+")))
