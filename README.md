## HWhile
HWhile is an interpreter for the While language written in Haskell. The While
language is a simple imperative programming language, with while-loops,
assignment and conditionals. It has a single data type: binary trees.

This implementation also includes syntactic sugar such as switch statements,
macros and natural numbers, all of which are translated into 'pure' while before
execution.

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

### Semantics
The semantic function for while programs takes a while program and a tree,
returning a tree. The result is the store value of y (the write variable) after
evaluating the block with the initial store where x (the read variable) has
value t (the input tree). The function is defined as follows:

    P[[ . ]] : Program -> Tree -> Tree
    P[[ n read x BLK write y ]] t = σ(y) where σ = B[[ BLK ]] [x -> t]

The semantic function for blocks takes a block and an initial store, returning
the store that results from the evaluation of the commands in the block with
the initial store.

    B[[ . ]] : Block -> Store -> Store
    B[[ {} ]] σ = σ
    B[[ { C1, C2, ..., CN } ]] σ = B[[ { C2, ...,CN } ]] ( C[[ C1 ]] σ )

The semantic function for commands takes a command and an initial store,
returning the store that results from the evaluation of the command the initial
store.

    C[[ . ]] : Command -> Store -> Store

To evaluate an assignment, we insert a mapping for x into the store, such that
the resulting store maps x to the result of evaluating the expression e in the
initial store.

    C[[ x := e ]] σ = σ[ x -> E[[ e ]] σ ]

    TBC
### Instructions
To run the project, you must first install cabal, and the alex and happy cabal
packages. Once these are installed, cd into the repo and run

    cabal build

to compile the project. The executable will be output to

    dist/build/hwhile/hwhile

and can therefore be run with the command:

    ./dist/build/hwhile/hwhile <FLAG> <FILE> <EXPR>

when at the root directory of the repo.
