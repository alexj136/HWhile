{
module Lexer
    ( Token (..)
    , TokenType (..)
    , tkLineNo
    , tkCharNo
    , tkPath
    , tkVarName
    , scan
    , prettyPrintToken
    ) where
}

%wrapper "posn"

$digit = 0-9
$lower = a-z
$upper = A-Z
$alpha = [$lower $upper]
$alnum = [$alpha $digit]
$inmac = [$alpha \_ \$]
$invar = [$inmac $digit]

tokens :-
    $white+               ; -- Ignore whitespace
    \/\/.*\n              ; -- // single line comments
    \(\*(.|\n)*\*\)       ; -- (* Multiline comments *)
    \.                    { \p s fp -> Token ( fp , TkDot           , p ) }
    \(                    { \p s fp -> Token ( fp , TkOpenBrc       , p ) }
    \)                    { \p s fp -> Token ( fp , TkClosBrc       , p ) }
    \{                    { \p s fp -> Token ( fp , TkOpenCur       , p ) }
    \}                    { \p s fp -> Token ( fp , TkClosCur       , p ) }
    \[                    { \p s fp -> Token ( fp , TkOpenSqu       , p ) }
    \]                    { \p s fp -> Token ( fp , TkClosSqu       , p ) }
    \<                    { \p s fp -> Token ( fp , TkOpenAng       , p ) }
    \>                    { \p s fp -> Token ( fp , TkClosAng       , p ) }
    \,                    { \p s fp -> Token ( fp , TkComma         , p ) }
    \:\=                  { \p s fp -> Token ( fp , TkAssign        , p ) }
    \:                    { \p s fp -> Token ( fp , TkColon         , p ) }
    \=                    { \p s fp -> Token ( fp , TkIsEq          , p ) }
    "nil"                 { \p s fp -> Token ( fp , TkNil           , p ) }
    \;                    { \p s fp -> Token ( fp , TkSemiCo        , p ) }
    "cons"                { \p s fp -> Token ( fp , TkCons          , p ) }
    "hd"                  { \p s fp -> Token ( fp , TkHd            , p ) }
    "tl"                  { \p s fp -> Token ( fp , TkTl            , p ) }
    "while"               { \p s fp -> Token ( fp , TkWhile         , p ) }
    "switch"              { \p s fp -> Token ( fp , TkSwitch        , p ) }
    "case"                { \p s fp -> Token ( fp , TkCase          , p ) }
    "default"             { \p s fp -> Token ( fp , TkDefault       , p ) }
    "if"                  { \p s fp -> Token ( fp , TkIf            , p ) }
    "else"                { \p s fp -> Token ( fp , TkElse          , p ) }
    "read"                { \p s fp -> Token ( fp , TkRead          , p ) }
    "write"               { \p s fp -> Token ( fp , TkWrite         , p ) }
    "true"                { \p s fp -> Token ( fp , TkTrue          , p ) }
    "false"               { \p s fp -> Token ( fp , TkFalse         , p ) }
    "@:="                 { \p s fp -> Token ( fp , TkAtomAsgn      , p ) }
    "@asgn"               { \p s fp -> Token ( fp , TkAtomAsgn      , p ) }
    "@doAsgn"             { \p s fp -> Token ( fp , TkAtomDoAsgn    , p ) }
    "@while"              { \p s fp -> Token ( fp , TkAtomWhile     , p ) }
    "@doWhile"            { \p s fp -> Token ( fp , TkAtomDoWhile   , p ) }
    "@if"                 { \p s fp -> Token ( fp , TkAtomIf        , p ) }
    "@doIf"               { \p s fp -> Token ( fp , TkAtomDoIf      , p ) }
    "@var"                { \p s fp -> Token ( fp , TkAtomVar       , p ) }
    "@quote"              { \p s fp -> Token ( fp , TkAtomQuote     , p ) }
    "@hd"                 { \p s fp -> Token ( fp , TkAtomHd        , p ) }
    "@doHd"               { \p s fp -> Token ( fp , TkAtomDoHd      , p ) }
    "@tl"                 { \p s fp -> Token ( fp , TkAtomTl        , p ) }
    "@doTl"               { \p s fp -> Token ( fp , TkAtomDoTl      , p ) }
    "@cons"               { \p s fp -> Token ( fp , TkAtomCons      , p ) }
    "@doCons"             { \p s fp -> Token ( fp , TkAtomDoCons    , p ) }
    $alpha[$invar]*       { \p s fp -> Token ( fp , ITkVar s        , p ) }
    "0"                   { \p s fp -> Token ( fp , ITkInt (read s) , p ) }
    [1-9][$digit]*        { \p s fp -> Token ( fp , ITkInt (read s) , p ) }

{

newtype Token = Token (FilePath, TokenType, AlexPosn) deriving Show

data TokenType
    = TkDot
    | TkOpenBrc
    | TkClosBrc
    | TkOpenCur
    | TkClosCur
    | TkOpenSqu
    | TkClosSqu
    | TkOpenAng
    | TkClosAng
    | TkComma
    | TkColon
    | TkIsEq
    | TkAssign
    | TkNil
    | TkSemiCo
    | TkCons
    | TkHd
    | TkTl
    | TkWhile
    | TkSwitch
    | TkCase
    | TkDefault
    | TkIf
    | TkElse
    | TkRead
    | TkWrite
    | TkTrue
    | TkFalse
    | TkAtomAsgn
    | TkAtomDoAsgn
    | TkAtomWhile
    | TkAtomDoWhile
    | TkAtomIf
    | TkAtomDoIf
    | TkAtomVar
    | TkAtomQuote
    | TkAtomHd
    | TkAtomDoHd
    | TkAtomTl
    | TkAtomDoTl
    | TkAtomCons
    | TkAtomDoCons
    | ITkVar String
    | ITkInt Int
    deriving (Show, Eq)

-- Main scanning function. Wraps makeGVars and alexScanTokens.
scan :: String -> FilePath -> [Token]
scan s fp = map (\t -> t fp) (alexScanTokens s)

-- The default implementation is not quite sufficient - it is more useful for
-- tokens to be equal regardless of position
instance Eq Token where
    (==) (Token (_, tyA, _)) (Token (_, tyB, _)) = tyA == tyB

-- Get the number of lines into the file that the text produced this token
-- occurred
tkLineNo :: Token -> Int
tkLineNo tok = case tok of Token (_, _, (AlexPn _ line char)) -> line

-- Get the number of characters into the line that the text that produced this
-- token occurred
tkCharNo :: Token -> Int
tkCharNo tok = case tok of Token (_, _, (AlexPn _ line char)) -> char

-- get the variable name in an ITkVar token. Fails if the token isn't an ITkVar.
tkVarName :: Token -> String
tkVarName (Token (_, ITkVar s, _)) = s
tkVarName _                        = error "Not an ITkVar token"

-- Get the file path that a token came from
tkPath :: Token -> FilePath
tkPath (Token (fp, _, _)) = fp

-- Get a pretty string representation of a token for error message purposes
prettyPrintToken :: Token -> String
prettyPrintToken t = case t of
    Token (_, TkDot         , _) -> "'.'"
    Token (_, TkOpenBrc     , _) -> "'('"
    Token (_, TkClosBrc     , _) -> "')'"
    Token (_, TkOpenCur     , _) -> "'{'"
    Token (_, TkClosCur     , _) -> "'}'"
    Token (_, TkOpenSqu     , _) -> "'['"
    Token (_, TkClosSqu     , _) -> "']'"
    Token (_, TkOpenAng     , _) -> "'<'"
    Token (_, TkClosAng     , _) -> "'>'"
    Token (_, TkComma       , _) -> "','"
    Token (_, TkIsEq        , _) -> "'='"
    Token (_, TkAssign      , _) -> "':='"
    Token (_, TkNil         , _) -> "'nil'"
    Token (_, TkSemiCo      , _) -> "';'"
    Token (_, TkCons        , _) -> "'cons'"
    Token (_, TkHd          , _) -> "'hd'"
    Token (_, TkTl          , _) -> "'tl'"
    Token (_, TkWhile       , _) -> "'while'"
    Token (_, TkIf          , _) -> "'if'"
    Token (_, TkElse        , _) -> "'else'"
    Token (_, TkRead        , _) -> "'read'"
    Token (_, TkWrite       , _) -> "'write'" 
    Token (_, TkTrue        , _) -> "'true'" 
    Token (_, TkFalse       , _) -> "'false'" 
    Token (_, TkAtomAsgn    , _) -> "'@:=' (or '@asgn')"
    Token (_, TkAtomDoAsgn  , _) -> "'@doAsgn'"
    Token (_, TkAtomWhile   , _) -> "'@while'"
    Token (_, TkAtomDoWhile , _) -> "'@doWhile'"
    Token (_, TkAtomIf      , _) -> "'@if'"
    Token (_, TkAtomDoIf    , _) -> "'@doIf'"
    Token (_, TkAtomVar     , _) -> "'@var'"
    Token (_, TkAtomQuote   , _) -> "'@quote'"
    Token (_, TkAtomHd      , _) -> "'@hd'"
    Token (_, TkAtomDoHd    , _) -> "'@doHd'"
    Token (_, TkAtomTl      , _) -> "'@tl'"
    Token (_, TkAtomDoTl    , _) -> "'@doTl'"
    Token (_, TkAtomCons    , _) -> "'@cons'"
    Token (_, TkAtomDoCons  , _) -> "'@doCons'"
    Token (_, ITkVar   s    , _) -> "variable  '" ++ s ++ "'"
    Token (_, ITkInt   i    , _) -> "integer  '" ++ show i ++ "'"
}
