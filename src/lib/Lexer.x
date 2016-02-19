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
    \.                    { \p s -> NearlyTok ( TkDot                    , p ) }
    \(                    { \p s -> NearlyTok ( TkOpenBrc                , p ) }
    \)                    { \p s -> NearlyTok ( TkClosBrc                , p ) }
    \{                    { \p s -> NearlyTok ( TkOpenCur                , p ) }
    \}                    { \p s -> NearlyTok ( TkClosCur                , p ) }
    \[                    { \p s -> NearlyTok ( TkOpenSqu                , p ) }
    \]                    { \p s -> NearlyTok ( TkClosSqu                , p ) }
    \<                    { \p s -> NearlyTok ( TkOpenAng                , p ) }
    \>                    { \p s -> NearlyTok ( TkClosAng                , p ) }
    \,                    { \p s -> NearlyTok ( TkComma                  , p ) }
    \:\=                  { \p s -> NearlyTok ( TkAssign                 , p ) }
    \:                    { \p s -> NearlyTok ( TkColon                  , p ) }
    \=                    { \p s -> NearlyTok ( TkIsEq                   , p ) }
    "nil"                 { \p s -> NearlyTok ( TkNil                    , p ) }
    \;                    { \p s -> NearlyTok ( TkSemiCo                 , p ) }
    "cons"                { \p s -> NearlyTok ( TkCons                   , p ) }
    "hd"                  { \p s -> NearlyTok ( TkHd                     , p ) }
    "tl"                  { \p s -> NearlyTok ( TkTl                     , p ) }
    "while"               { \p s -> NearlyTok ( TkWhile                  , p ) }
    "switch"              { \p s -> NearlyTok ( TkSwitch                 , p ) }
    "case"                { \p s -> NearlyTok ( TkCase                   , p ) }
    "default"             { \p s -> NearlyTok ( TkDefault                , p ) }
    "if"                  { \p s -> NearlyTok ( TkIf                     , p ) }
    "else"                { \p s -> NearlyTok ( TkElse                   , p ) }
    "read"                { \p s -> NearlyTok ( TkRead                   , p ) }
    "write"               { \p s -> NearlyTok ( TkWrite                  , p ) }
    "true"                { \p s -> NearlyTok ( TkTrue                   , p ) }
    "false"               { \p s -> NearlyTok ( TkFalse                  , p ) }
    "@:="                 { \p s -> NearlyTok ( TkAtomAsgn               , p ) }
    "@asgn"               { \p s -> NearlyTok ( TkAtomAsgn               , p ) }
    "@doAsgn"             { \p s -> NearlyTok ( TkAtomDoAsgn             , p ) }
    "@while"              { \p s -> NearlyTok ( TkAtomWhile              , p ) }
    "@doWhile"            { \p s -> NearlyTok ( TkAtomDoWhile            , p ) }
    "@if"                 { \p s -> NearlyTok ( TkAtomIf                 , p ) }
    "@doIf"               { \p s -> NearlyTok ( TkAtomDoIf               , p ) }
    "@var"                { \p s -> NearlyTok ( TkAtomVar                , p ) }
    "@quote"              { \p s -> NearlyTok ( TkAtomQuote              , p ) }
    "@hd"                 { \p s -> NearlyTok ( TkAtomHd                 , p ) }
    "@doHd"               { \p s -> NearlyTok ( TkAtomDoHd               , p ) }
    "@tl"                 { \p s -> NearlyTok ( TkAtomTl                 , p ) }
    "@doTl"               { \p s -> NearlyTok ( TkAtomDoTl               , p ) }
    "@cons"               { \p s -> NearlyTok ( TkAtomCons               , p ) }
    "@doCons"             { \p s -> NearlyTok ( TkAtomDoCons             , p ) }
    $alpha[$invar]*       { \p s -> NearlyTok ( ITkVar s                 , p ) }
    "0"                   { \p s -> NearlyTok ( ITkInt (read s)          , p ) }
    [1-9][$digit]*        { \p s -> NearlyTok ( ITkInt (read s)          , p ) }

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
scan :: FilePath -> String -> [Token]
scan fp s = map (completeToken fp) (alexScanTokens s)

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

-- Get the file path that a token came from
tkPath :: Token -> FilePath
tkPath (Token (fp, _, _)) = fp

-- get the variable name in an ITkVar token. Fails if the token isn't an ITkVar.
tkVarName :: Token -> String
tkVarName (Token (_, ITkVar s, _)) = s
tkVarName _                        = error "Not an ITkVar token"

-- Complete NearlyTokens into Tokens by adding FilePath info
completeToken :: FilePath -> NearlyTok -> Token
completeToken fp (NearlyTok (ty, pos)) = Token (fp, ty, pos)

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
    Token (_, TkAtomAsgn    , _) -> "'@asgn'"
    Token (_, TkAtomDoAsgn  , _) -> "'@do_asgn'"
    Token (_, TkAtomWhile   , _) -> "'@while'"
    Token (_, TkAtomDoWhile , _) -> "'@do_while'"
    Token (_, TkAtomIf      , _) -> "'@if'"
    Token (_, TkAtomDoIf    , _) -> "'@do_if'"
    Token (_, TkAtomVar     , _) -> "'@var'"
    Token (_, TkAtomQuote   , _) -> "'@quote'"
    Token (_, TkAtomHd      , _) -> "'@hd'"
    Token (_, TkAtomDoHd    , _) -> "'@do_hd'"
    Token (_, TkAtomTl      , _) -> "'@tl'"
    Token (_, TkAtomDoTl    , _) -> "'@do_tl'"
    Token (_, TkAtomCons    , _) -> "'@cons'"
    Token (_, TkAtomDoCons  , _) -> "'@do_cons'"
    Token (_, ITkVar   s    , _) -> "variable  '" ++ s ++ "'"
    Token (_, ITkInt   i    , _) -> "integer  '" ++ show i ++ "'"
}
