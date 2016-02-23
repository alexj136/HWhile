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
    IsEq a b        -> error "Unparse equality expression"

unparseName :: VarMap -> Name -> ETree
unparseName vm n = maybe (error "Unparse VarMap miss") intToTree (M.lookup n vm)

removeEquality :: Program -> Program
removeEquality (Program x blk y) = Program x (removeEqualityBlock blk) y

removeEqualityBlock :: Block -> Block
removeEqualityBlock = concat . map removeEqualityComm

removeEqualityComm :: Command -> Block
removeEqualityComm comm = case comm of
    Assign v x     -> undefined
    While  x b     -> undefined
    IfElse x bt bf -> undefined

replaceEqualities :: Expression -> (Expression, [(Name, Expression, Expression)])
replaceEqualities exp = case exp of
    Var  _   -> (exp, [])
    Lit  _   -> (exp, [])
    Hd   x   -> let (rX, subs) = replaceEqualities x in (Hd rX, subs)
    Tl   x   -> let (rX, subs) = replaceEqualities x in (Tl rX, subs)
    Cons a b -> let
        (rA, subsA) = replaceEqualities a
        (rB, subsB) = replaceEqualities b in
            (Cons rA rB, subsA ++ subsB)
    IsEq a b -> let
        (rA, subsA) = replaceEqualities a
        (rB, subsB) = replaceEqualities b in
            (Var undefined, (undefined, a, b) : subsA ++ subsB)

equalityTester :: Command
equalityTester = IfElse (Lit ENil) []
    [ asgn equals (Lit (intToTree 1))
    , whilev stack
        [ asgn next  (Hd (Var (iname stack)))
        , asgn stack (Tl (Var (iname stack)))
        , asgn a     (Hd (Var (iname next)))
        , asgn b     (Hd (Tl (Var (iname next))))
        , ifv a
            [ ifv b
                [ asgn stack (Cons (Cons (Hd (v a)) (Cons (Hd (v b)) (Lit ENil))) (v stack))
                , asgn stack (Cons (Cons (Tl (v a)) (Cons (Tl (v b)) (Lit ENil))) (v stack))
                ]
                [ asgn stack  (Lit ENil)
                , asgn equals (Lit ENil)
                ]
            ]
            [ ifv b
                [ asgn stack  (Lit ENil)
                , asgn equals (Lit ENil)
                ] []
            ]
        ]
    ]
    where
    a :: String
    a = "+NEXT+A+"

    b :: String
    b = "+NEXT+B+"

    next :: String
    next = "+NEXT+"

    stack :: String
    stack = "+STACK+"

    equals :: String
    equals = "+EQUALS+"

    iname :: String -> Name
    iname n = Name ("+IMPL+", n)

    v :: String -> Expression
    v = Var . iname

    asgn :: String -> Expression -> Command
    asgn n = Assign (iname n)

    ifv :: String -> Block -> Block -> Command
    ifv n = IfElse (Var (iname n))

    whilev :: String -> Block -> Command
    whilev n = While (Var (iname n))
