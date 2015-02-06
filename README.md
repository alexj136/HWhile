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

    PROG ::= read ID; CMD; write ID

    CMD  ::= CMD; CMD
           | ID := EXP
           | while EXP do { CMD }
           | if EXP then { CMD } else { CMD }

    EXP  ::= nil
           | cons EXP EXP
           | EXP.EXP
           | hd EXP
           | head EXP
           | tl EXP
           | tail EXP
           | (EXP)
           | ?= EXP EXP
           | ID
           | NAT
           | []
           | [ EXP LIST

    LIST ::= , EXP LIST
           | ]

    NAT  ::= [0-9]+

    ID   ::= [a-zA-Z_'][a-zA-Z0-9_']*

There are several additions to the 'pure' syntax given on page 32 of Neil Jones'
book:
- conditional (if-then-else) commands - these are converted into assignments and
loops. To see how, look in the file Parser.y.
- An infix cons operator, '.'.
- Haskell/Python style list literals, which are converted into the standard
while list encoding - see page 35 of Neil Jones' book.
- Natural numbers, which are converted into the standard natural number
encoding - see page 36 of Neil Jones' book.

### Instructions
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

### Implementation description
*The following description is at present a plan, rather than the actual state of
the system.*

The interpreter transforms program representation in several stages prior to
interpretation. These stages are as follows:
- **Lexical analysis/scanning**. This phase transforms source code programs into
lists of tokens, where variable names are represented by strings.
- **Name conversion**. This phase transforms the representation of variable names
from strings to integers. A map from integer names to string names, used for 
printing, is output along with the tokens.
tokens. This map
- **Parsing**. In this phase, the token list is converted into a syntax tree
with the full complement of syntactic sugar, i.e. pattern-matching and
conditional branching.
- **Desugaring**. Here, pattern-matching and conditional constructs are
converted into pure while-loops and assignments.
