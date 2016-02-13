## HWhile
HWhile is an interpreter for the While language written in Haskell. The While
language is a simple imperative programming language, with while-loops,
assignment, and a single data type: binary trees. The language was concieved by
Neil D. Jones in his book: Computability and Complexity from a Programming
Perspective, available [here](http://www.diku.dk/~neil/Comp2book.html). This
implementation is loosely based on the language described in the book. See the
examples directory for some example code.

### Syntax
The grammar below gives exactly the concrete syntax of this implementation:

    PROG  ::= read ID BLOCK write ID
    
    BLOCK ::= { CMD }
            | {}

    CMD   ::= CMD; CMD                      // Sequential composition
            | ID := EXP                     // Assignment
            | while EXP { CMD }             // While loops
            | if EXP { CMD } else { CMD }   // If-then-else statements
            | if EXP { CMD }                // If-then statements
            | switch EXP { CASES            // Switch statements
            | ID := <ID> EXP                // Macro calls

    CASES ::= case EXP : CMD CASES
            | default : CMD }
            | }

    EXP   ::= nil
            | cons EXP EXP
            | EXP.EXP
            | hd EXP
            | tl EXP
            | (EXP)
            | EXP = EXP
            | ID
            | NAT
            | []
            | [ EXP LIST
          
    LIST  ::= , EXP LIST
            | ]
          
    NAT   ::= 0|[1-9][0-9]+
          
    ID    ::= [a-zA-Z_'][a-zA-Z0-9_']*

### Instructions
To run the project, you must first install cabal, and the alex and happy cabal
packages. Once these are installed, cd into the repo and run

    cabal build

to compile the project. The executable will be output to

    dist/build/hwhile/hwhile

and can therefore be run with the command:

    ./dist/build/hwhile/hwhile <FLAG> <FILE> <EXPR>

when at the root directory of the repo.
