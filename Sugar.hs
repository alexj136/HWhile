module Sugar where

{- This module defines a higher-level version of the syntax for commands,
 - including with conditional and pattern-matching commands. This module is
 - included for easier parsing and translation to pure syntax.
 -}

import Syntax

data SuCommand
    = SuCompos SuCommand SuCommand
    | SuAssign String Expression
    | SuWhile  Expression SuCommand
    | SuIfElse Expression SuCommand SuCommand
    | SuPMatch Expression [(Expression, SuCommand)]
    deriving Eq

-- Desugar a command, that is, convert it to pure while syntax
desugar :: SuCommand -> Command
desugar suComm = case suComm of
    SuCompos c1 c2    -> Compos (desugar c1) (desugar c2)
    SuAssign x  exp   -> Assign x exp
    SuWhile  gd c     -> While gd (desugar c)
    SuIfElse gd c1 c2 ->
        Compos (Compos (Compos (Compos (Compos
            (Assign "+NOT+EXP+STACK+" (Cons (Cons Nil Nil) (Var "+NOT+EXP+STACK+")))
            (Assign "+EXP+VAL+STACK+" (Cons gd (Var "+EXP+VAL+STACK+"))))
            (While (Hd (Var "+EXP+VAL+STACK+")) (Compos (Compos
                (Assign "+EXP+VAL+STACK+" (Cons Nil (Tl (Var "+EXP+VAL+STACK+"))))
                (Assign "+NOT+EXP+STACK+" (Cons Nil (Tl (Var "+NOT+EXP+STACK+")))))
                c1)))
            (While (Hd (Var "+NOT+EXP+STACK+")) (Compos
                (Assign "+NOT+EXP+STACK+" (Cons Nil (Tl (Var "+NOT+EXP+STACK+"))))
                c2)))
            (Assign "+NOT+EXP+STACK+" (Tl (Var "+NOT+EXP+STACK+"))))
            (Assign "+EXP+VAL+STACK+" (Tl (Var "+EXP+VAL+STACK+")))
