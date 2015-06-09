module HWhileUtils where

newtype Name a = Name a
    deriving (Eq, Ord)

instance Show Name where
    show :: Name String -> String
    show (Name s) = show s
