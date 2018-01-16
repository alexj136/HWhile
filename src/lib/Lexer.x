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

%wrapper "monadUserState"

$digit = 0-9
$lower = a-z
$upper = A-Z
$alpha = [$lower $upper]
$alnum = [$alpha $digit]
$inmac = [$alpha \_ \$]
$invar = [$inmac $digit]

tokens :-
    <0>       $white+         { skip                    }
    <0>       \/\/.*\n        { skip                    }
    <0>       "(*"            { skip `andBegin` comment }
    <comment> "*)"            { skip `andBegin` 0       }
    <comment> .|\n            { skip                    }
    <0>       "."             { makeToken TkDot         }
    <0>       "("             { makeToken TkOpenBrc     }
    <0>       ")"             { makeToken TkClosBrc     }
    <0>       "{"             { makeToken TkOpenCur     }
    <0>       "}"             { makeToken TkClosCur     }
    <0>       "["             { makeToken TkOpenSqu     }
    <0>       "]"             { makeToken TkClosSqu     }
    <0>       "<"             { makeToken TkOpenAng     }
    <0>       ">"             { makeToken TkClosAng     }
    <0>       ","             { makeToken TkComma       }
    <0>       ":="            { makeToken TkAssign      }
    <0>       ":"             { makeToken TkColon       }
    <0>       "="             { makeToken TkIsEq        }
    <0>       "nil"           { makeToken TkNil         }
    <0>       ";"             { makeToken TkSemiCo      }
    <0>       "cons"          { makeToken TkCons        }
    <0>       "hd"            { makeToken TkHd          }
    <0>       "tl"            { makeToken TkTl          }
    <0>       "while"         { makeToken TkWhile       }
    <0>       "switch"        { makeToken TkSwitch      }
    <0>       "case"          { makeToken TkCase        }
    <0>       "default"       { makeToken TkDefault     }
    <0>       "if"            { makeToken TkIf          }
    <0>       "else"          { makeToken TkElse        }
    <0>       "read"          { makeToken TkRead        }
    <0>       "write"         { makeToken TkWrite       }
    <0>       "true"          { makeToken TkTrue        }
    <0>       "false"         { makeToken TkFalse       }
    <0>       "@:="           { makeToken TkAtomAsgn    }
    <0>       "@asgn"         { makeToken TkAtomAsgn    }
    <0>       "@doAsgn"       { makeToken TkAtomDoAsgn  }
    <0>       "@while"        { makeToken TkAtomWhile   }
    <0>       "@doWhile"      { makeToken TkAtomDoWhile }
    <0>       "@if"           { makeToken TkAtomIf      }
    <0>       "@doIf"         { makeToken TkAtomDoIf    }
    <0>       "@var"          { makeToken TkAtomVar     }
    <0>       "@quote"        { makeToken TkAtomQuote   }
    <0>       "@hd"           { makeToken TkAtomHd      }
    <0>       "@doHd"         { makeToken TkAtomDoHd    }
    <0>       "@tl"           { makeToken TkAtomTl      }
    <0>       "@doTl"         { makeToken TkAtomDoTl    }
    <0>       "@cons"         { makeToken TkAtomCons    }
    <0>       "@doCons"       { makeToken TkAtomDoCons  }
    <0>       $alpha[$invar]* { makeVar                 }
    <0>       "0"             { makeInt                 }
    <0>       [1-9][$digit]*  { makeInt                 }
    <0>       .               { makeErr                 }

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
    | ITkErr String
    | TkEOF
    deriving (Show, Eq)

-- The default implementation is not quite sufficient - it is more useful for
-- tokens to be equal regardless of position
instance Eq Token where
    (==) (Token (fpA, tyA, _)) (Token (fpB, tyB, _)) = (fpA, tyA) == (fpB, tyB)

newtype AlexUserState = AlexUserState () deriving Show

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState ()

makeToken :: TokenType -> AlexInput -> Int -> Alex (FilePath -> Token)
makeToken ty (p, _, _, s) len = return $ \fp -> Token (fp, ty, p)

makeVar :: AlexInput -> Int -> Alex (FilePath -> Token)
makeVar (p, _, _, s) len = return $ \fp -> Token (fp, ITkVar (take len s), p)

makeInt :: AlexInput -> Int -> Alex (FilePath -> Token)
makeInt (p, _, _, s) len =
    return $ \fp -> Token (fp, ITkInt (read (take len s)), p)

makeErr :: AlexInput -> Int -> Alex (FilePath -> Token)
makeErr (p, _, _, s) len = return $ \fp -> Token (fp, ITkErr (take len s), p)

alexEOF :: Alex (FilePath -> Token)
alexEOF = return $ \fp -> Token (fp, TkEOF, undefined)

scan :: String -> FilePath -> [Token]
scan s fp = case runAlex s loop of 
    Left  err -> error err
    Right res -> map (\r -> r fp) res
    where
    loop = do
        tk <- alexMonadScan
        case tk (error "EOF") of
            Token (_, TkEOF, _) ->
                return []
            Token _             -> do
                tks <- loop
                return $ tk:tks

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
    Token (_, ITkErr   s    , _) -> s
    Token (_, TkEOF         , _) -> "EOF"
}
