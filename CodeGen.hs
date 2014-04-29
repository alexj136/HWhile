module CodeGen where

import Syntax
import Data.List (intersperse)

definitions :: String
definitions = concat $ intersperse "\n" $
    [ ""
    , ""
    , "#include <stdio.h>"
    , "#include <malloc.h>"
    , ""
    , "typedef enum {"
    , "    nil,"
    , "    cons"
    , "} NodeType;"
    , ""
    , "typedef struct Node Node;"
    , "struct Node {"
    , "    NodeType nodeType;"
    , "    Node *left;"
    , "    Node *right;"
    , "};"
    , ""
    , "Node *newNil() {"
    , "    Node *nilNode = malloc(sizeof(Node));"
    , "    nilNode->nodeType = nil;"
    , "    return nilNode;"
    , "}"
    ]

codeGenProg :: Program -> [String]
codeGenProg (Program rd comm wrt) = "codeGenProgram not yet implemented"

codeGenComm :: Command -> [String]
codeGenComm _ = error "codeGenCommand not yet implemented"

codeGenExpr :: Expression -> [String]
codeGenExpr _ = error "codeGenExpression not yet implemented"
