module Unparser
    ( unparse
    ) where

import Data.List (intersperse)
import qualified Data.Map as M
import PureSyntax

-- A VarMap is a Data.Map from variable names to integers for a program, for use
-- in list representations of programs.
type VarMap = M.Map Name Int

-- Create a VarMap for the given Program
varMapProg :: Program -> VarMap
varMapProg (Program rd comm wrt) =
    varMapName (varMapComm (M.singleton rd 0) comm) wrt

-- Extend the given VarMap with variables in the given Command
varMapComm :: VarMap -> Command -> VarMap
varMapComm vm comm = case comm of
    Compos a b -> varMapComm (varMapComm vm a) b
    Assign v x -> varMapExpr (varMapName vm v) x
    While  x c -> varMapComm (varMapExpr vm x) c

-- Extend the given VarMap with variables in the given Expression
varMapExpr :: VarMap -> Expression -> VarMap
varMapExpr vm expr = case expr of
    Var  s   -> varMapName vm s
    Nil      -> vm
    Cons a b -> varMapExpr (varMapExpr vm a) b
    Hd   x   -> varMapExpr vm x
    Tl   x   -> varMapExpr vm x
    IsEq a b -> varMapExpr (varMapExpr vm a) b

varMapName :: VarMap -> Name -> VarMap
varMapName vm n | M.member n vm = vm
                | otherwise     = M.insert n ((succ . maximum . M.elems) vm) vm

unparse :: Program -> String
unparse p = unparseProg 0 (varMapProg p) p

unparseProg :: Int -> VarMap -> Program -> String
unparseProg ts vm (Program x c y) = concat $ intersperse "\n"
    [ "[ " ++ show (vm M.! x)
    , ", " ++ unparseComm (succ ts) vm c
    , ", " ++ show (vm M.! y)
    , "] "
    ]

unparseComm :: Int -> VarMap -> Command -> String
unparseComm ts vm comm = case comm of
    While e c -> undefined
