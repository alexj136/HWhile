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
#### Installing Prerequisites
You will need GHC (the Haskell compiler) and Cabal (the Haskell build system)
installed on your machine in order to compile & run HWhile.
##### Windows/Mac
GHC and Cabal are included in the
[Haskell Platform](http://www.haskell.org/platform/) which you should download
and install.
##### Linux
As with Windows and Mac, you can download and install the
[Haskell Platform](http://www.haskell.org/platform/), however it is recommended
to install GHC (the haskell compiler) and Cabal (Haskell's build system) from
your distribution's repositories. For Debian/Ubuntu-based Distros, run:

    sudo apt-get install haskell-platform

For Arch Linux, run:

    sudo pacman -S ghc cabal-install

#### Compilation
To compile HWhile, first download HWhile (either using `git clone` or by
clicking the 'Download ZIP' button on HWhile's
[Github Page](https://github.com/alexj136/hwhile). Extract the downloaded files
(if necessary).

Next, `cd` into the root directory of the repository, and run the following
commands:

    cabal configure
    cabal build

These commands will download the required libraries (if necessary), and
compile HWhile. If this fails, try running

    cabal install --only-dependencies

and then run `cabal build` again.

#### Invocation
Compiling HWhile generates an executable file in the following directory:

    dist/build/hwhile/hwhile        ( on Mac & Linux )
    dist/build/hwhile/hwhile.exe    ( on Windows     )

relative to the root directory of the repository. It can then be run with the
command:

    ./dist/build/hwhile/hwhile <FLAG> <FILE> <EXPR>        ( Mac & Linux )
    ./dist/build/hwhile/hwhile.exe <FLAG> <FILE> <EXPR>    ( Windows     )

For example:

    ./dist/build/hwhile/hwhile -i examples/count.while "[1, 2, 3]"         ( Mac & Linux )
    ./dist/build/hwhile/hwhile.exe -i examples/count.while "[1, 2, 3]"     ( Windows     )

This example takes a list of numbers as its argument and outputs their sum, so
you should see `6` as the output.
