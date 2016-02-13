module Unparser
    ( unparse
    , unparseWithNames
    ) where

import qualified Data.Map as M
import PureSyntax

type NameEnv a = ((M.Map Name Int, Int), a)

unparse :: Program -> String
unparse = unparseWithNames M.empty 1

getName :: NameEnv Name -> NameEnv String
getName ((map, next), name) | M.member name map = ((map, next), map M.! name)
getName ((map, next), name) | otherwise         =
    ((M.insert name next map, succ next), next)

unparseWithNames :: NameEnv Program -> NameEnv String
unparseWithNames (Program (Name (_, x) c e) = concat $
    [ "[ [ @var, 1 ]"
    , ", "
    ]
