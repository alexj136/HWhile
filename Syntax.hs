module Syntax where

import qualified Data.Map as M

-- Syntax definitions for while programs

data Program = Program String [Command] Expression
    deriving Eq

data Command = Assign String     Expression
             | While  Expression [Command]
    deriving Eq

data Expression = Var  String
                | Nil
                | Cons Expression Expression
                | Hd   Expression
                | Tl   Expression
                | IsEq Expression Expression
    deriving Eq

instance Show Program where
    show (Program r cs w) = "read " ++ r ++ ";\n" ++
                           showCs 0 cs ++
                           "write " ++ show w

instance Show Command where
    show c = showC 0 c

showCs :: Int -> [Command] -> String
showCs _ []     = ""
showCs i (c:cs) = (tabs i) ++ show c
                  ++ showCs i cs

showC :: Int -> Command -> String
showC i (While x cs) = (tabs i) ++ "while " ++ show x ++ " do:\n"
                       ++ showCs (i + 1) cs
showC i (Assign v x) = (tabs i) ++ v ++ " := " ++ show x ++ ";\n"

tabs :: Int -> String
tabs x | x <  0 = error "negative tabs"
       | x == 0 = ""
       | x >  0 = "    " ++ tabs (x - 1)

instance Show Expression where
    show (Var  s  ) = s
    show (Nil     ) = "nil"
    show (Cons a b) = '(' : show a ++ '.' : show b ++ ")"
    show (Hd   x  ) = "hd " ++ show x
    show (Tl   x  ) = "tl " ++ show x
    show (IsEq a b) = "=? " ++ show a ++ ' ' : show b
