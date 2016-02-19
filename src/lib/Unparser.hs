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
    Compos a b     -> varMapComm (varMapComm vm a) b
    Assign v x     -> varMapExpr (varMapName vm v) x
    While  x c     -> varMapComm (varMapExpr vm x) c
    IfElse e c1 c2 -> varMapExpr (varMapComm (varMapComm vm c2) c1) e

-- Extend the given VarMap with variables in the given Expression
varMapExpr :: VarMap -> Expression -> VarMap
varMapExpr vm expr = case expr of
    Var  s   -> varMapName vm s
    Lit  _   -> vm
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
unparseProg ts vm (Program x c y) = concat $ intersperse "\n" $ map (tabs ts ++)
    [ "[ " ++ show (vm M.! x)
    , ", " ++ unparseComm (succ ts) vm c
    , ", " ++ show (vm M.! y)
    , "]"
    ]

unparseComm :: Int -> VarMap -> Command -> String
unparseComm ts vm comm = tabs ts ++ case comm of
    Compos a b     -> unparseCommList ts vm (flatten comm)
    Assign v x     -> "[ @asgn , " ++ show (vm M.! v) ++ " , "
                   ++ unparseExpr ts vm x ++ " ]"
    While x c      -> "[ @while , " ++ unparseExpr ts vm x ++ " , "
                   ++ unparseComm ts vm c ++ " ]"
    IfElse e c1 c2 -> "[ @if , " ++ unparseExpr ts vm e ++ " , "
                   ++ unparseComm ts vm c1 ++ " , "
                   ++ unparseComm ts vm c2 ++ " ]"

-- Unfold a sequential composition into a list of commands
flatten :: Command -> [Command]
flatten c = case c of { Compos c1 c2 -> flatten c1 ++ flatten c2 ; _ -> [c] }

unparseCommList :: Int -> VarMap -> [Command] -> String
unparseCommList _  _  []     = "[]"
unparseCommList ts vm (c:cs) = undefined

unparseExpr :: Int -> VarMap -> Expression -> String
unparseExpr ts vm expr = undefined
