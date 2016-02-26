## HWhile
HWhile is an interpreter for a simple While language originally used by Neil
Jones in his book “Computability and Complexity". The While language is a simple
imperative programming language with while-loops, assignment and conditionals.
It has a single data type: binary trees. The HWhile interpreter is written in
Haskell.

This current implementation also includes syntactic sugar such as switch
statements, macros, list notation, equality, and natural number literals. It
therefore supports (almost fully) the version of While used in Bernhard Reus’
textbook [Limits of Computation - From a Programming Perspective](http:limits.bernhardreus.com)
and has been developed in co-operation with Bernhard to support students on the
corresponding module at Sussex University.
This version also allows one to translate while programs into programs as data.
For this to work,  all the syntax sugar (extensions) of a While program has to
be removed again.

More about the syntax and the semantics (and usage) of the While language can be
found in Bernhard’s textbook (Chapter 3-5).

### Syntax
The grammar below gives exactly the concrete syntax of this implementation:

    PROG  ::= ID read ID BLOCK write ID
    
    BLOCK ::= { CMDS }
            | {}

    CMDS  ::= CMD ; CMD
            | CMD

    CMD   ::= ID := EXP                     // Assignment
            | while EXP BLOCK               // While loops
            | if EXP BLOCK else BLOCK       // If-then-else statements
            | if EXP BLOCK                  // If-then statements
            | ID := <ID> EXP                // Macro calls
            | switch EXP { CASES            // Switch statements

    CASES ::= case EXP : CMDS CASES
            | default : CMDS }
            | }

    EXP   ::= LIT
            | cons EXP EXP
            | hd EXP
            | tl EXP
            | (EXP)
            | EXP = EXP
            | ID
            | []
            | [ EXP LIST

    LIST  ::= , EXP LIST
            | ]
          
    LIT   ::= nil
            | true
            | false
            | <LIT.LIT>
            | NAT
            | @asgn
            | @:=
            | @doAsgn
            | @while
            | @doWhile
            | @if
            | @doIf
            | @var
            | @quote
            | @hd
            | @doHd
            | @tl
            | @doTl
            | @cons
            | @doCons
          
    NAT   ::= 0|[1-9][0-9]+
          
    ID    ::= [a-zA-Z_'][a-zA-Z0-9_']*

Command line input must conform to the following grammar:

    INP    ::= nil
            | true
            | false
            | <LIT.LIT>
            | NAT
            | []
            | [ INP INPLST
            | @asgn
            | @:=
            | @doAsgn
            | @while
            | @doWhile
            | @if
            | @doIf
            | @var
            | @quote
            | @hd
            | @doHd
            | @tl
            | @doTl
            | @cons
            | @doCons

    INPLST ::= , INP INPLST
            | ]

### Instructions
To run the project, you must first install cabal, and the alex and happy cabal
packages. Once these are installed, cd into the repo and run

    cabal build

to compile the package. The executable will be output to

    dist/build/hwhile/hwhile

and can therefore be run with the command:

    ./dist/build/hwhile/hwhile <FLAG> <FILE> <EXPR>

when at the root directory of the repo.
