HWhile is an interpreter for the While language written in Haskell.

See 'example.while' for a simple example.

SYNTAX

The parser uses a grammar identical to that in the lecture slides, with four
exceptions:

    - You can use an infix constructor: You can write 'a . b' as well as
      'cons a b', if you prefer

    - Brackets can be used to enforce a particular association

    - The grammar in the lecture slides requires us to write a variable at the
      end of our programs. The parser's grammar allows us to put an expression
      here, as well as a variable. This makes it possible to write constant
      functions, or functions that modify the returned value at the end of the
      program, in a more obvious way, e.g:

       read X; Y := nil . nil; write Y

      can be written as

       read X; X := X; write nil . nil

    - Integer literals can be used - the parser will convert them into the
      appropriate tree before execution

The lexer also provides us with some nice things:

    - A '#' will tell the lexer to ignore the rest of the line, e.g. write a
      comment

    - the head & tail functions can be written fully ('head' or 'tail') or in
      the abbreviated fashion ('hd' and 'tl')

SEMANTICS

The semantics of this implementation match exactly those specified in the
lecture slides (in theory - there will inevitably be bugs).

INSTRUCTIONS

To compile for usage purposes, cd into the repo directory and run

    ghc --make Main -o hwhile

This should work on any computer with GHC installed. You can then run

Windows:

    hwhile <FLAG> <FILE> <EXPR>

Mac/Linux:

    ./hwhile <FLAG> <FILE> <EXPR>

to invoke the interpreter. If you want to modify the source code, you will need
to have the following installed:
    
    - make

    - HUnit (Haskell unit testing library)

    - alex (Haskell lexer generator)

    - happy (Haskell parser generator)
