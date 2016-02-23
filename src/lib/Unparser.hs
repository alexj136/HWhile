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

unparse :: Program -> ETree
unparse p = unparseProg (varMapProg p) p

unparseProg :: VarMap -> Program -> ETree
unparseProg vm (Program x b y) =
    treeFromHaskellList [unparseName vm x, unparseBlock vm b, unparseName vm y]

unparseBlock :: VarMap -> Block -> ETree
unparseBlock vm blk = treeFromHaskellList $ map (unparseComm vm) blk

unparseComm :: VarMap -> Command -> ETree
unparseComm vm comm = treeFromHaskellList $ case comm of
    Assign v x     ->
        [ atomToTree AtomAsgn
        , unparseName vm v
        , unparseExpr vm x
        ]
    While  x b     ->
        [ atomToTree AtomWhile
        , unparseExpr vm x
        , unparseBlock vm b
        ]
    IfElse x bt bf ->
        [ atomToTree AtomIf
        , unparseExpr vm x
        , unparseBlock vm bt
        , unparseBlock vm bf
        ]

unparseExpr :: VarMap -> Expression -> ETree
unparseExpr vm expr = treeFromHaskellList $ case expr of
    Lit (ECons a b) ->
        [ atomToTree AtomCons
        , unparseExpr vm (Lit a)
        , unparseExpr vm (Lit b)
        ]
    Lit  ENil       ->
        [ atomToTree AtomQuote
        , ENil
        ]
    Var  s          ->
        [ atomToTree AtomVar
        , unparseName vm s
        ]
    Cons a b        ->
        [ atomToTree AtomCons
        , unparseExpr vm a
        , unparseExpr vm b
        ]
    Hd   x          ->
        [ atomToTree AtomHd
        , unparseExpr vm x
        ]
    Tl   x          ->
        [ atomToTree AtomTl
        , unparseExpr vm x
        ]
    IsEq a b        -> undefined

unparseName :: VarMap -> Name -> ETree
unparseName vm n = maybe (error "Unparse VarMap miss") intToTree (M.lookup n vm)
