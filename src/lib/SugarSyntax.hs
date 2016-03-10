module SugarSyntax
    ( SuProgram (..)
    , SuCommand (..)
    , SuBlock
    ) where

{- This module defines a higher-level version of the syntax for commands,
 - including conditional, macro and switch commands. This module is included for
 - easier parsing and translation to pure syntax.
 -}

import qualified PureSyntax as Pure

data SuProgram = SuProgram Pure.Name Pure.Name SuBlock Pure.Name deriving Eq

type Expression = Pure.Expression
type Command = Pure.Command
type Block = Pure.Block
 
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
