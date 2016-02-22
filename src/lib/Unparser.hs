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
varMapProg (Program rd blk wrt) =
    varMapName (varMapBlock (M.singleton rd 0) blk) wrt

-- Extend the given VarMap with variables in the given Block
varMapBlock :: VarMap -> Block -> VarMap
varMapBlock vm []     = vm
varMapBlock vm (c:cs) = varMapBlock (varMapComm vm c) cs

-- Extend the given VarMap with variables in the given Command
varMapComm :: VarMap -> Command -> VarMap
varMapComm vm comm = case comm of
    Assign v x     -> varMapExpr (varMapName vm v) x
    While  x b     -> varMapBlock (varMapExpr vm x) b
    IfElse e bt bf -> varMapExpr (varMapBlock (varMapBlock vm bf) bt) e

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
unparseProg ts vm (Program x b y) = concat $ intersperse "\n" $ map (tabs ts ++)
    [ "[ " ++ show (vm M.! x)
    , ", " ++ unparseBlock (succ ts) vm b
    , ", " ++ show (vm M.! y)
    , "]"
    ]

unparseBlock :: Int -> VarMap -> Block -> String
unparseBlock ts vm [] = "[]"
unparseBlock ts vm l  = "{\n" ++
    (concat $ intersperse ";\n" $ map (unparseComm (ts + 1) vm) l)
    ++ "\n"
    ++ (tabs ts) ++ "}"

unparseComm :: Int -> VarMap -> Command -> String
unparseComm ts vm comm = tabs ts ++ case comm of
    Assign v x     -> "[ @asgn , " ++ show (vm M.! v) ++ " , "
                   ++ unparseExpr ts vm x ++ " ]"
    While x b      -> "[ @while , " ++ unparseExpr ts vm x ++ " , "
                   ++ unparseBlock ts vm b ++ " ]"
    IfElse e bt bf -> "[ @if , " ++ unparseExpr ts vm e ++ " , "
                   ++ unparseBlock ts vm bt ++ " , "
                   ++ unparseBlock ts vm bf ++ " ]"

unparseExpr :: Int -> VarMap -> Expression -> String
unparseExpr ts vm expr = undefined
