# HWhile
HWhile is an interpreter for the While language written in Haskell. The While
language is a simple imperative programming language, with while-loops,
assignment, and a single data type: binary trees. The language was concieved by
Neil D. Jones in his book: Computability and Complexity from a Programming
Perspective, available [here](http://www.diku.dk/~neil/Comp2book.html). See the
examples directory for some example code.

## Instructions

To compile for usage purposes, cd into the repo directory and run

> ghc --make Main -o hwhile

This should work on any computer with GHC installed. You can then run

Windows:

> hwhile <FLAG> <FILE> <EXPR>

Mac/Linux:

> ./hwhile <FLAG> <FILE> <EXPR>

to invoke the interpreter. If you want to modify the source code, you will need
to have the following installed:
    
- make
- HUnit (Haskell unit testing library)
- alex (Haskell lexer generator)
- happy (Haskell parser generator)
