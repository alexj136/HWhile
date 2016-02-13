module PureSyntax where

import qualified Data.Map as M

-- Syntax definitions for while programs. The data types below match the
-- context-free grammar in Neil Jones' book, page 32. This module also contains
-- functions for printing syntax trees.

newtype Name = Name (FilePath, String) deriving (Eq, Ord)

data Program
    = Program Name Command Expression
    deriving (Eq, Ord)

data Command
    = Compos Command    Command
    | Assign Name       Expression
    | While  Expression Command
    | IfElse Expression Command Command
    deriving (Eq, Ord)

data Expression
    = Var  Name  
    | Nil
    | Cons Expression Expression
    | Hd   Expression
    | Tl   Expression
    | IsEq Expression Expression
    deriving (Eq, Ord)

instance Show Name where
    show (Name (fp, x)) = "<<" ++ x ++ " of " ++ fp ++ ">>"

instance Show Program where
    show (Program n c w) = "read " ++ (show n) ++ " {\n"
                        ++ (show c) ++ "\n"
                        ++ "} write " ++ (show w)

instance Show Command where
    show c = showC 0 c

showC :: Int -> Command -> String
showC i (While  x c)   = (tabs i) ++ "while " ++ show x ++ " {\n"
                      ++ showC (i + 1) c ++ "\n"
                      ++ (tabs i) ++ "}"
showC i (Assign v x)   = (tabs i) ++ (show v) ++ " := " ++ show x
showC i (Compos a b)   = (showC i a) ++ ";\n"
                      ++ (showC i b)
showC i (IfElse e a b) = (tabs i) ++ "if " ++ show e ++ " { " ++ show a
                      ++ " } else { " ++ show b ++ " }"

tabs :: Int -> String
tabs x | x <  0 = error "negative tabs"
       | x == 0 = ""
       | x >  0 = "    " ++ tabs (x - 1)

instance Show Expression where
    show (Var  s   ) = show s
    show (Nil      ) = "nil"
    show (Cons a b ) = '(' : show a ++ '.' : show b ++ ")"
    show (Hd   x   ) = "hd " ++ show x
    show (Tl   x   ) = "tl " ++ show x
    show (IsEq a b ) = show a ++ " = " ++ show b

-- Convert a while integer expression into a decimal number string. If the
-- isVerbose argument is True, unparsable expressions will be displayed in full.
-- If it is False, unparsable expressions yield "E".
showIntExp :: Bool -> Expression -> String
showIntExp isVerbose exp = case parseInt exp of
    Just i              -> show i
    Nothing | isVerbose -> show exp
    Nothing | otherwise -> "E"

-- Parse an Int from a while Expression. Not all while expressions encode
-- integers, so return a value in the Maybe monad.
parseInt :: Expression -> Maybe Int
parseInt = parseIntAcc 0
    where
    parseIntAcc :: Int -> Expression -> Maybe Int
    parseIntAcc acc Nil          = Just acc
    parseIntAcc acc (Cons Nil x) = parseIntAcc (acc + 1) x
    parseIntAcc acc _            = Nothing

-- Convert a while expression encoded list into a haskell list
toActualList :: Expression -> [Expression]
toActualList = reverse . (toActualListAcc [])
    where
    toActualListAcc :: [Expression] -> Expression -> [Expression]
    toActualListAcc acc exp = case exp of
        Nil              -> acc
        (Cons elem rest) -> toActualListAcc (elem : acc) rest
        _                -> error "Cannot print unreduced expression"
