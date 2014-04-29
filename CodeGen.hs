module CodeGen where

import Syntax
import Data.List (intersperse)
import qualified Data.Map as M

-- A VarMap is a Data.Map from variable names to integers for a program. When
-- compiling, integer variable names are far simpler to use than strings.
type VarMap = M.Map String Int

-- Create a VarMap for the given Program
varMapProg :: Program -> VarMap
varMapProg (Program rd comm wrt) =
    varMapExpr (varMapComm (M.singleton rd 0) comm) wrt

-- Extend the given VarMap with variables in the given Command
varMapComm :: VarMap -> Command -> VarMap
varMapComm vm comm = case comm of
    Compos a b -> varMapComm (varMapComm vm a) b
    Assign v x ->
        if M.member v vm then
            varMapExpr vm x
        else
            varMapExpr (M.insert v ((maximum (M.elems vm)) + 1) vm) x
    While  x c -> varMapComm (varMapExpr vm x) c

-- Extend the given VarMap with variables in the given Expression
varMapExpr :: VarMap -> Expression -> VarMap
varMapExpr vm expr = case expr of
    Var s | M.member s vm -> vm
          | otherwise     -> M.insert s ((maximum (M.elems vm)) + 1) vm
    Nil                   -> vm
    Cons a b              -> varMapExpr (varMapExpr vm a) b
    Hd   x                -> varMapExpr vm x
    Tl   x                -> varMapExpr vm x
    IsEq a b              -> varMapExpr (varMapExpr vm a) b

codeGenProg :: Program -> [String]
codeGenProg (Program rd comm wrt) =
    [  ""
    ,  ""
    ,  "#include <stdio.h>"
    ,  "#include \"whilelib.h\""
    ,  "int main() {"
    ,  "    Node *tmp = NULL;"
    ,  "    Node **store = setUpVars(" ++ show (maximum (M.elems vm)) ++ ");"
    ] ++ codeGenComm vm comm ++
    [  ""
    ,  "}"
    ]
  where
    vm = varMapProg (Program rd comm wrt)

codeGenComm :: VarMap -> Command -> [String]
codeGenComm vm comm = case comm of
    While  x c ->
        [  "// begin compile while " ++ show x ++ " do"
        ,  "tmp = " ++ codeGenExpr vm x ++ ";"
        ,  "while(tmp->nodeType == cons) {"
        ,  "free(tmp);"
        ,  "tmp = NULL;"
        , codeGenComm vm c
        ,  "tmp = " ++ codeGenExpr vm x ++ ";"
        ,  "}"
        ,  "free(tmp);"
        ,  "tmp = NULL;"
        , "// end compile while"
        ]
    Assign v x ->
        [  "// compile " ++ v ++ " := " ++ show x
        ,  "tmp = store[" ++ show (vm ! v) ++ "];"
        ,  "store[" ++ show (vm ! v) ++ "] = " ++ codeGenExpr vm x ++ ";"
        ,  "freeTree(tmp);"
        ,  "tmp = NULL;"
        ]
    Compos a b ->
        [ codeGenComm vm a
        , codeGenComm vm b
        ]

codeGenExpr :: VarMap -> Expression -> String
codeGenExpr vm expr = case expr of
    Var  s   -> "copyTree(store[" ++ show (vm ! s) ++ "])"
    Nil      -> "newNil()"
    Cons a b -> "takeCons(" ++ gen a ++ ", " ++ gen b ++ ")"
    Hd   x   -> "takeHead(" ++ gen x ++ ")"
    Tl   x   -> "takeTail(" ++ gen x ++ ")"
    IsEq a b -> "treeEqual(" ++ gen a ++ ", " ++ gen b ++ ")?\
        \takeCons(newNil(), newNil()):newNil()"
  where gen = codeGenExpr vm
