module Desugar ( loadProg , desugarProg ) where

import qualified Data.Set as S
import System.FilePath (pathSeparator)
import Parser (parseProg)
import Lexer (scan)
import PureSyntax
import SugarSyntax

-- Given its directory, base name, a macro call stack and macro seed, load a
-- program from disk, returning it with the macro seed after unparsing etc.
-- Also check for macro recursion or non-matching file+prog names, failing if
-- either is found.
loadProg ::
    FilePath   -> -- The directory we look for files to open
    FilePath   -> -- The file we want to load
    [FilePath] -> -- The 'macro stack'. Prevents recursion thusly: every time we
                  -- recurse into loading a new macro, we push the name of the
                  -- previous macro on the stack, and then check that the
                  -- new macro is not already somewhere on the stack. If it is,
                  -- we've found recursion so we quit. Otherwise, we pop the
                  -- current macro off the stack when we finish it.
    IO Program    -- The loaded program
loadProg dir fileBaseName macroStack =
    if fileBaseName `elem` macroStack then
        error "Recursive macros detected."
    else do
        fileStr <- readFile $ dir ++ pathSeparator : fileBaseName ++ ".while"
        let fileTokens  = scan fileStr fileBaseName
        let suProg      = parseProg fileTokens
        case suProg of
            SuProgram n _ _ _ | nameName n /= fileBaseName -> error $
                "Program name (" ++ nameName n ++ ") must match file base name."
            SuProgram n r b w ->
                let namesToInit = S.delete r $ S.insert w $ namesSuBlock b
                    initCode    = map (\n -> SuAssign n (Lit ENil)) $
                        S.toList namesToInit
                in desugarProg dir ( fileBaseName : macroStack )
                    (SuProgram n r (initCode ++ b) w)

-- Desugar a program, that is, convert it to pure while syntax
desugarProg :: FilePath -> [FilePath] -> SuProgram -> IO Program
desugarProg dir macroStack ( SuProgram n r blk w ) = do
    desugaredBlk <- desugarBlock dir macroStack blk
    return $ Program n r desugaredBlk w

-- Desugar a block
desugarBlock :: FilePath -> [FilePath] -> SuBlock -> IO Block
desugarBlock dir _          []         = return []
desugarBlock dir macroStack ( c : cs ) = do
    desugaredC  <- desugarComm  dir macroStack c
    desugaredCs <- desugarBlock dir macroStack cs
    return $ desugaredC ++ desugaredCs

-- Desugar a command
desugarComm ::
    FilePath   -> -- Path to search for macro files
    [FilePath] -> -- Macro call stack
    SuCommand  -> -- The command to desugar
    IO Block
desugarComm dir macroStack suComm = case suComm of
    SuAssign x exp -> return [ Assign x exp ]
    SuWhile gd blk -> do
        desugaredBlk <- desugarBlock dir macroStack blk
        return [ While gd desugaredBlk ]
    SuIfElse gd bt bf -> do
        desugaredBT <- desugarBlock dir macroStack bt
        desugaredBF <- desugarBlock dir macroStack bf
        return [ IfElse gd desugaredBT desugaredBF ]
    Macro x f e -> do
        prog <- loadProg dir f macroStack
        return $
            [ Assign ( readVar prog ) e ] ++
            block prog ++
            [ Assign x ( Var ( writeVar prog ) ) ]
    Switch e [] def -> desugarBlock dir macroStack def
    Switch e ( ( matchE , blk ) : cases ) def -> do
        desugaredBlk  <- desugarBlock dir macroStack blk
        desugaredRest <- desugarComm  dir macroStack ( Switch e cases def )
        return [ IfElse ( IsEq e matchE ) desugaredBlk desugaredRest ]
