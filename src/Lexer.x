{
module Lexer where
}

%wrapper "posn"

$digit = 0-9
$lower = a-z
$upper = A-Z
$alpha = [$lower $upper]
$alnum = [$alpha $digit]
$inmac = [$alpha \. \\ \/]

tokens :-
    $white+               ; -- Ignore whitespace
    \#.*\n                ; -- Ignore the rest of a line after '#'
    \.                    { \p s -> Token ( TkDot                    , p ) } -- Infix cons
    \(                    { \p s -> Token ( TkOpenBrc                , p ) }
    \)                    { \p s -> Token ( TkClosBrc                , p ) }
    \{                    { \p s -> Token ( TkOpenCur                , p ) }
    \}                    { \p s -> Token ( TkClosCur                , p ) }
    \[                    { \p s -> Token ( TkOpenSqu                , p ) }
    \]                    { \p s -> Token ( TkClosSqu                , p ) }
    \,                    { \p s -> Token ( TkComma                  , p ) }
    \?\=                  { \p s -> Token ( TkIsEq                   , p ) }
    \:\=                  { \p s -> Token ( TkAssign                 , p ) }
    \:                    { \p s -> Token ( TkColon                  , p ) }
    "nil"                 { \p s -> Token ( TkNil                    , p ) }
    \;                    { \p s -> Token ( TkSemiCo                 , p ) }
    "cons"                { \p s -> Token ( TkCons                   , p ) } -- Prefix cons
    "hd"                  { \p s -> Token ( TkHead                   , p ) }
    "head"                { \p s -> Token ( TkHead                   , p ) }
    "tl"                  { \p s -> Token ( TkTail                   , p ) }
    "tail"                { \p s -> Token ( TkTail                   , p ) }
    "while"               { \p s -> Token ( TkWhile                  , p ) }
    "switch"              { \p s -> Token ( TkSwitch                 , p ) }
    "case"                { \p s -> Token ( TkCase                   , p ) }
    "default"             { \p s -> Token ( TkDefault                , p ) }
    "if"                  { \p s -> Token ( TkIf                     , p ) }
    "else"                { \p s -> Token ( TkElse                   , p ) }
    "read"                { \p s -> Token ( TkRead                   , p ) }
    "write"               { \p s -> Token ( TkWrite                  , p ) }
    $alpha[$alnum \_ \']* { \p s -> Token ( ITkVar s                 , p ) }
    "0"                   { \p s -> Token ( ITkInt (read s)          , p ) }
    [1-9][$digit]*        { \p s -> Token ( ITkInt (read s)          , p ) }
    \<[$inmac]*\>         { \p s -> Token ( ITkMacro (init (tail s)) , p ) }

{

newtype Token = Token (TokenType, AlexPosn)

data TokenType
    = TkDot
    | TkOpenBrc
    | TkClosBrc
    | TkOpenCur
    | TkClosCur
    | TkOpenSqu
    | TkClosSqu
    | TkComma
    | TkColon
    | TkIsEq
    | TkAssign
    | TkNil
    | TkSemiCo
    | TkCons
    | TkHead
    | TkTail
    | TkWhile
    | TkSwitch
    | TkCase
    | TkDefault
    | TkIf
    | TkElse
    | TkRead
    | TkWrite
    | ITkVar   String
    | ITkInt   Int
    | ITkMacro FilePath
    deriving (Show, Eq)

-- The default implementation is not quite sufficient - it is more useful for
-- tokens to be equal regardless of position
instance Eq Token where
    (==) (Token (tyA, _)) (Token (tyB, _)) = tyA == tyB

-- Get the number of lines into the file that the text produced this token
-- occurred
lineNo :: Token -> Int
lineNo tok = case tok of Token (_, (AlexPn _ line char)) -> line

-- Get the number of characters into the line that the text that produced this
-- token occurred
charNo :: Token -> Int
charNo tok = case tok of Token (_, (AlexPn _ line char)) -> char

-- Get a string representation of a token for error message purposes
instance Show Token where
    show t = case t of
        Token (TkDot      , _) -> "'.'"
        Token (TkOpenBrc  , _) -> "'('"
        Token (TkClosBrc  , _) -> "')'"
        Token (TkOpenCur  , _) -> "'{'"
        Token (TkClosCur  , _) -> "'}'"
        Token (TkOpenSqu  , _) -> "'['"
        Token (TkClosSqu  , _) -> "']'"
        Token (TkComma    , _) -> "','"
        Token (TkIsEq     , _) -> "'?='"
        Token (TkAssign   , _) -> "':='"
        Token (TkNil      , _) -> "'nil'"
        Token (TkSemiCo   , _) -> "';'"
        Token (TkCons     , _) -> "'cons'"
        Token (TkHead     , _) -> "'head'"
        Token (TkTail     , _) -> "'tail'"
        Token (TkWhile    , _) -> "'while'"
        Token (TkIf       , _) -> "'if'"
        Token (TkElse     , _) -> "'else'"
        Token (TkRead     , _) -> "'read'"
        Token (TkWrite    , _) -> "'write'"
        Token (ITkVar   s , _) -> "variable '" ++ s ++ "'"
        Token (ITkInt   i , _) -> "integer '" ++ show i ++ "'"
        Token (ITkMacro f , _) -> "macro <" ++ f ++ ">"
}
