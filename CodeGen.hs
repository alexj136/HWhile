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

codeGenProg :: Program -> String
codeGenProg (Program rd comm wrt) = concat $ intersperse "\n" $
    [ "#include <stdio.h>"
    , "#include \"whilelib.h\""
    , "int main() {"
    , "    // Set up variables"
    , "    Node *tmp = NULL;"
    , "    Node **store = setUpStore(" ++ show (maximum (M.elems vm)) ++ ");"
    , ""
    ] ++ codeGenComm 1 vm comm ++
    [ "    printTreeLinear(" ++ codeGenExpr vm wrt ++ ");"
    , "    putchar('\\n');"
    , "    freeStore(" ++ show (maximum (M.elems vm)) ++ ", store);"
    , "    return 0;"
    , "}"
    ]
    where vm = varMapProg (Program rd comm wrt)

codeGenComm :: Int -> VarMap -> Command -> [String]
codeGenComm i vm comm = case comm of
    While  x c ->
        [ tabs i ++ "// begin compile while " ++ show x ++ " do"
        , tabs i ++ "tmp = " ++ gen x ++ ";"
        , tabs i ++ "while(tmp->nodeType == cons) {"
        , tabs i ++ "freeTree(tmp);"
        , tabs i ++ "tmp = NULL;"
        ] ++ codeGenComm (i + 1) vm c ++
        [ tabs i ++ "tmp = " ++ gen x ++ ";"
        , tabs i ++ "}"
        , tabs i ++ "freeTree(tmp);"
        , tabs i ++ "tmp = NULL;"
        , tabs i ++ "// end compile while"
        ]
    Assign v x ->
        [ tabs i ++ "// compile " ++ v ++ " := " ++ show x
        , tabs i ++ "tmp = store[" ++ show (vm M.! v) ++ "];"
        , tabs i ++ "store[" ++ show (vm M.! v) ++ "] = " ++ gen x ++ ";"
        , tabs i ++ "freeTree(tmp);"
        , tabs i ++ "tmp = NULL;"
        , ""
        ]
    Compos a b -> codeGenComm i vm a ++ codeGenComm i vm b
    where gen = codeGenExpr vm

codeGenExpr :: VarMap -> Expression -> String
codeGenExpr vm expr = case expr of
    Var  s   -> "copyTree(store[" ++ show (vm M.! s) ++ "])"
    Nil      -> "newNil()"
    Cons a b -> "newCons(" ++ gen a ++ ", " ++ gen b ++ ")"
    Hd   x   -> "takeHead(" ++ gen x ++ ")"
    Tl   x   -> "takeTail(" ++ gen x ++ ")"
    IsEq a b -> "treeEqual(" ++ gen a ++ ", " ++ gen b ++ ")?\
        \newCons(newNil(), newNil()):newNil()"
    where gen = codeGenExpr vm
