{
module Lexer
    ( Token (..)
    , TokenType (..)
    , lineNo
    , charNo
    , pathOf
    , varName
    , scan
    ) where
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
    \.                    { \p s -> NearlyTok ( TkDot                    , p ) }
    \(                    { \p s -> NearlyTok ( TkOpenBrc                , p ) }
    \)                    { \p s -> NearlyTok ( TkClosBrc                , p ) }
    \{                    { \p s -> NearlyTok ( TkOpenCur                , p ) }
    \}                    { \p s -> NearlyTok ( TkClosCur                , p ) }
    \[                    { \p s -> NearlyTok ( TkOpenSqu                , p ) }
    \]                    { \p s -> NearlyTok ( TkClosSqu                , p ) }
    \,                    { \p s -> NearlyTok ( TkComma                  , p ) }
    \?\=                  { \p s -> NearlyTok ( TkIsEq                   , p ) }
    \:\=                  { \p s -> NearlyTok ( TkAssign                 , p ) }
    \:                    { \p s -> NearlyTok ( TkColon                  , p ) }
    "nil"                 { \p s -> NearlyTok ( TkNil                    , p ) }
    \;                    { \p s -> NearlyTok ( TkSemiCo                 , p ) }
    "cons"                { \p s -> NearlyTok ( TkCons                   , p ) }
    "hd"                  { \p s -> NearlyTok ( TkHead                   , p ) }
    "head"                { \p s -> NearlyTok ( TkHead                   , p ) }
    "tl"                  { \p s -> NearlyTok ( TkTail                   , p ) }
    "tail"                { \p s -> NearlyTok ( TkTail                   , p ) }
    "while"               { \p s -> NearlyTok ( TkWhile                  , p ) }
    "switch"              { \p s -> NearlyTok ( TkSwitch                 , p ) }
    "case"                { \p s -> NearlyTok ( TkCase                   , p ) }
    "default"             { \p s -> NearlyTok ( TkDefault                , p ) }
    "if"                  { \p s -> NearlyTok ( TkIf                     , p ) }
    "else"                { \p s -> NearlyTok ( TkElse                   , p ) }
    "read"                { \p s -> NearlyTok ( TkRead                   , p ) }
    "write"               { \p s -> NearlyTok ( TkWrite                  , p ) }
    $alpha[$alnum \_ \']* { \p s -> NearlyTok ( ITkVar s                 , p ) }
    "0"                   { \p s -> NearlyTok ( ITkInt (read s)          , p ) }
    [1-9][$digit]*        { \p s -> NearlyTok ( ITkInt (read s)          , p ) }
    \<[$inmac]*\>         { \p s -> NearlyTok ( ITkMacro (init (tail s)) , p ) }

{

newtype Token = Token (FilePath, TokenType, AlexPosn) deriving Show

newtype NearlyTok = NearlyTok (TokenType, AlexPosn) deriving Show

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
    | ITkInt   Int                -- GVar tokens, with the filepath they came
    | ITkMacro FilePath           -- from. Prevents name clashes with macros.
    deriving (Show, Eq)

-- Main scanning function. Wraps makeGVars and alexScanTokens.
scan :: FilePath -> String -> [Token]
scan fp s = map (completeToken fp) (alexScanTokens s)

-- The default implementation is not quite sufficient - it is more useful for
-- tokens to be equal regardless of position
instance Eq Token where
    (==) (Token (_, tyA, _)) (Token (_, tyB, _)) = tyA == tyB

-- Get the number of lines into the file that the text produced this token
-- occurred
lineNo :: Token -> Int
lineNo tok = case tok of Token (_, _, (AlexPn _ line char)) -> line

-- Get the number of characters into the line that the text that produced this
-- token occurred
charNo :: Token -> Int
charNo tok = case tok of Token (_, _, (AlexPn _ line char)) -> char

-- Get the file path that a token came from
pathOf :: Token -> FilePath
pathOf (Token (fp, _, _)) = fp

-- get the variable name in an ITkVar token. Fails if the token isn't an ITkVar.
varName :: Token -> String
varName (Token (_, ITkVar s, _)) = s
varName _                       = error "Not an ITkVar token"

-- Complete NearlyTokens into Tokens by adding FilePath info
completeToken :: FilePath -> NearlyTok -> Token
completeToken fp (NearlyTok (ty, pos)) = Token (fp, ty, pos)

-- Get a pretty string representation of a token for error message purposes
prettyPrintToken :: Token -> String
prettyPrintToken t = case t of
    Token (_, TkDot      , _) -> "'.'"
    Token (_, TkOpenBrc  , _) -> "'('"
    Token (_, TkClosBrc  , _) -> "')'"
    Token (_, TkOpenCur  , _) -> "'{'"
    Token (_, TkClosCur  , _) -> "'}'"
    Token (_, TkOpenSqu  , _) -> "'['"
    Token (_, TkClosSqu  , _) -> "']'"
    Token (_, TkComma    , _) -> "','"
    Token (_, TkIsEq     , _) -> "'?='"
    Token (_, TkAssign   , _) -> "':='"
    Token (_, TkNil      , _) -> "'nil'"
    Token (_, TkSemiCo   , _) -> "';'"
    Token (_, TkCons     , _) -> "'cons'"
    Token (_, TkHead     , _) -> "'head'"
    Token (_, TkTail     , _) -> "'tail'"
    Token (_, TkWhile    , _) -> "'while'"
    Token (_, TkIf       , _) -> "'if'"
    Token (_, TkElse     , _) -> "'else'"
    Token (_, TkRead     , _) -> "'read'"
    Token (_, TkWrite    , _) -> "'write'" 
    Token (_, ITkVar   s , _) -> "variable  '" ++ s ++ "'"
    Token (_, ITkInt   i , _) -> "integer  '" ++ show i ++ "'"
    Token (_, ITkMacro f , _) -> "macro <" ++ f ++ ">"
}
