module Unparser
    ( unparse
    ) where

import Data.List (intersperse)
import qualified Data.Map as M
import PureSyntax

unparse :: Program -> ETree
unparse p =
    let (vmNoEq, pNoEq) = removeEquality (varMapProg p) p in
        unparseProg vmNoEq pNoEq

--------------------------------------------------------------------------------
-- VarMap generation functions
--------------------------------------------------------------------------------

-- A VarMap is a Data.Map from variable names to integers for a program, for use
-- in list representations of programs.
type VarMap = M.Map Name Int

-- Create a VarMap for the given Program
varMapProg :: Program -> VarMap
varMapProg (Program _ rd blk wrt) =
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

--------------------------------------------------------------------------------
-- Unparsing functions - require equality to already be removed
--------------------------------------------------------------------------------

unparseProg :: VarMap -> Program -> ETree
unparseProg vm (Program _ x b y) =
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

--------------------------------------------------------------------------------
-- Equality removal functions - require a VarMap for the given program
--------------------------------------------------------------------------------

-- To remove equality from a program, remove it from the block
removeEquality :: VarMap -> Program -> (VarMap, Program)
removeEquality vm (Program n x blk y) =
    let (vmB, rBlk) = removeEqualityBlock vm blk in (vmB, Program n x rBlk y)

-- To remove equality from a block, remove it from each command in the block
removeEqualityBlock :: VarMap -> Block -> (VarMap, Block)
removeEqualityBlock vm []     = (vm, [])
removeEqualityBlock vm (c:cs) =
    let (vmC , cBlk) = removeEqualityComm  vm  c
        (vmCs, rBlk) = removeEqualityBlock vmC cs in
            (vmCs, cBlk ++ rBlk)

-- To remove equality from a command, replace all equality expressions in
-- contained expressions with new variables, and assign to those variables the
-- result of the equalities, prior to the command itself.
removeEqualityComm :: VarMap -> Command -> (VarMap, Block)
removeEqualityComm vm comm = case comm of
    Assign v x     -> case removeEqualityExpr vm x of
        (_  , _ , [] ) -> (vm ,        [ Assign v x  ])
        (vmX, rX, blk) -> (vmX, blk ++ [ Assign v rX ])
    While  x b     -> let (vmB, blkN) = removeEqualityBlock vm b in
        case removeEqualityExpr vmB x of
            (_   , _ , []  ) -> (vmB ,         [ While x  blkN ])
            (vmBX, rX, blkX) -> (vmBX, blkX ++ [ While rX blkN ])
    IfElse x bt bf -> let
        (vmBT  , blkT) = removeEqualityBlock vm   bt
        (vmBTBF, blkF) = removeEqualityBlock vmBT bf in
        case removeEqualityExpr vmBTBF x of
            (_      , _ , []  ) -> (vmBTBF ,         [ IfElse x  blkT blkF ])
            (vmBTBFX, rX, blkX) -> (vmBTBFX, blkX ++ [ IfElse rX blkT blkF ])

-- Insert variables in place of equality expressions, returning the new
-- expression and a list of commands to evaluate equality, leaving the result in
-- the inserted variable.
removeEqualityExpr ::
    VarMap     ->   -- the initial VarMap
    Expression ->   -- the expression to substitute
    ( VarMap        -- the final VarMap
    , Expression    -- the replacement expression (always a Var)
    , Block         -- Commands to evaluate the equality
    )
removeEqualityExpr vm exp = case exp of
    Var  _ -> (vm, exp, [])
    Lit  _ -> (vm, exp, [])
    Hd   x -> let (vmX, rX, blk) = removeEqualityExpr vm x in (vmX, Hd rX, blk)
    Tl   x -> let (vmX, rX, blk) = removeEqualityExpr vm x in (vmX, Tl rX, blk)
    Cons a b -> let
        (vmA , rA, blkA) = removeEqualityExpr vm  a
        (vmAB, rB, blkB) = removeEqualityExpr vmA b in
            (vmAB, Cons rA rB, blkA ++ blkB)
    IsEq a b -> let
        (vmA , rA, blkA) = removeEqualityExpr vm  a
        (vmAB, rB, blkB) = removeEqualityExpr vmA b
        nameNum          = (succ . maximum . M.elems) vm
        name             = iname ("+EQ+" ++ show nameNum ++ "+")
        vmABN            = M.insert name nameNum vmAB
        (vmABNE, eqComm) = equalityTester vmABN name
        in
        (vmABNE, Var name, blkA ++ blkB ++
            [ Assign (iname stack) (lst [lst [rA, rB]])
            , eqComm
            ])

-- Generate a command that tests equality using a stack. Requires the stack set
-- up as a singleton list containing a two element list, where the inner list
-- contains either side of the equality expression. Once the command has run,
-- the variable with the given name will be 0 if not equal, or 1 if they are.
equalityTester :: VarMap -> Name -> (VarMap, Command)
equalityTester vm equals = let
    vmNew = foldl (\m n -> varMapName m (iname n)) vm [stack, next, a, b]
    in (vmNew, IfElse (Lit ENil) []
        [ Assign equals (Lit (intToTree 1))
        , whilev stack
            [ asgn next  (Hd (v stack))
            , asgn stack (Tl (v stack))
            , asgn a     (Hd (v next ))
            , asgn b     (Hd (Tl (v next)))
            , ifv a
                [ ifv b
                    [ asgn stack $ Cons (lst [Hd (v a), Hd (v b)]) (v stack)
                    , asgn stack $ Cons (lst [Tl (v a), Tl (v b)]) (v stack)
                    ]
                    [ asgn   stack  (Lit ENil)
                    , Assign equals (Lit ENil)
                    ]
                ]
                [ ifv b
                    [ asgn   stack  (Lit ENil)
                    , Assign equals (Lit ENil)
                    ] []
                ]
            ]
        ])
    where
    -- AST helper functions

    a :: String
    a = "+NEXT+A+"

    b :: String
    b = "+NEXT+B+"

    next :: String
    next = "+NEXT+"

    v :: String -> Expression
    v = Var . iname

    asgn :: String -> Expression -> Command
    asgn n = Assign (iname n)

    ifv :: String -> Block -> Block -> Command
    ifv n = IfElse (Var (iname n))

    whilev :: String -> Block -> Command
    whilev n = While (Var (iname n))

lst :: [Expression] -> Expression
lst []     = Lit ENil
lst (e:es) = Cons e (lst es)

iname :: String -> Name
iname n = Name ("+IMPL+", n)

stack :: String
stack = "+STACK+"
