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
found in Bernhard’s textbook (Chapter 3-5), and we also include a summary below.

### Instructions

#### Installing Prerequisites
All the tools required to compile and run HWhile are included in the
[Haskell Platform](http://www.haskell.org/platform/). Make sure you install the
_full_ version as opposed to the _core_ version.

Note that you may need to add the Haskell Platform's binaries to your system's
path variable. If you're on windows this should happen automatically. Otherwise
some configuration may be required.

#### Installing HWhile
Once the Haskell Platform is installed and configured correctly, you can install
HWhile by running:

    stack install hwhile

This will download HWhile and its depenencies (if necessary) and compile and
install them.

#### Invocation
If installed correctly, HWhile can be run with the command:

    hwhile <FLAG> <FILE> <EXPR>

For example:

    hwhile -i examples/count.while "[1, 2, 3]"

This example takes a list of numbers as its argument and outputs their sum, so
you should see `6` as the output.

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

### Semantics
The semantics of HWhile programs are defined by functions. These functions take
a program construct - an expression, block, command or program, and a _store_ -
a function of type `X -> T` that maps variable names to trees.

We first define two functions over stores - addition, which adds a new binding,
and lookup, which retrieves the tree bound to the given name.

    σ + (x -> T) = σ, x -> T       if x ∉ σ
                 | σ', x -> T, σ'' if σ = σ', x -> U, σ''

    σ(x) = T   if σ = σ', x -> T, σ''
         | nil if x ∉ σ

Semantics for expressions are defined as follows:

    [e] : EXP -> (X -> T) -> T

    [x] σ = σ(x)
    [hd e] σ = nil if [e] σ = nil
             | H   if [e] σ = <H.T>
    [tl e] σ = nil if [e] σ = nil
             | T   if [e] σ = <H.T>
    [e = e'] σ = nil if [e] σ /= [e'] σ
               | T   if [e] σ  = [e'] σ where T /= nil
    [cons e e'] σ = <H.T> where H = [e] σ, T = [e'] σ

For blocks:

    [b] : BLOCK -> (X -> T) -> (X -> T)

    [{}] σ = σ
    [{c; cs}] σ = [{cs}] ([c] σ)

For commands, we define semantics only for pure commands (assignment, while
loops and if-then-else statements). The semantics for switch statements and
macro calls are given by translation to pure commands. The semantics for pure
commands are given as follows:

    [c] : CMD -> (X -> T) -> (X -> T)

    [x := e] σ = σ + (x -> [e] σ)
    [while e b] σ = σ                   if [e] σ  = nil
                  | [while e b] ([b] σ) if [e] σ /= nil
    [if e b else b'] σ = [b]  σ if [e] σ /= nil
                       | [b'] σ if [e] σ  = nil

The translations for if-then statements, switch statements and macros are
defined as follows:

    if e b = if e b else {}

    switch e {} = {}
    switch e { default : cs } = { cs }
    switch e1 { case e2 : cs cases } =
        { if e1 = e2 { cs } else switch e1 { cases } }

    x := <macro> e = z := e; disjoin(cmds, z, y); x := y
    where macro.while = macro read z { cmds } write y

The function `disjoin` renames the variable names in `cmds` such that there are
no names in common with the program in which the macro call occurs, except for
the read-variable `z` and the write-variable `y`.

Finally, the semantics for programs are defined as follows:

    [p] : PROG -> T -> T

    [n read x b write y] T = ([b] (x -> T))(y)
