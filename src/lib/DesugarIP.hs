module DesugarIP ( desugarProg ) where

import PureSyntax
import InterSyntax

-- Desugar a program, that is, convert it to pure while syntax
desugarProg :: InProgram -> Program
desugarProg ( InProgram n r blk w ) = Program n r (desugarBlock blk) w

-- Desugar a block
desugarBlock :: InBlock -> Block
desugarBlock = concat . map desugarComm

-- Desugar a command
desugarComm :: InCommand -> Block
desugarComm suComm = case suComm of
    InAssign _ x exp -> [ Assign x exp ]
    InWhile  _ gd blk -> [ While gd (desugarBlock blk) ]
    InIfElse _ gd bt bf -> [ IfElse gd (desugarBlock bt) (desugarBlock bf) ]
    InSwitch _ e [] def -> desugarBlock def
    InSwitch i e ( ( matchE , blk ) : cases ) def -> let
        desugaredBlk  = desugarBlock blk
        desugaredRest = desugarComm  ( InSwitch i e cases def )
        in [ IfElse ( IsEq e matchE ) desugaredBlk desugaredRest ]
