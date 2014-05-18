module Sugar where

{- This module defines a higher-level version of the syntax for commands,
 - including with conditional and pattern-matching commands. This module is
 - included for easier parsing and translation to pure syntax.
 -}

import Syntax

data SuCommand
    = Compos SuCommand SuCommand
    | Assign String Expression
    | While  Expression SuCommand
    | IfElse Expression SuCommand SuCommand
    | PMatch Expression [(Expression, SuCommand)]
    deriving Eq
