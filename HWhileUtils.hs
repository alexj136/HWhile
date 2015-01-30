module HWhileUtils where

newtype Name a = Name a

instance Show (Name String) where
    show :: Name String -> String
    show (Name s) = s

instance Show (Name Int) where
    show :: Name Int -> String
    show (Name x) = "Var " ++ show x
