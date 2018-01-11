module DesugarSI ( loadProg , desugarProg ) where

import System.FilePath (pathSeparator)
import SourceParser (parseProg)
import Lexer (scan)
import PureSyntax
import InterSyntax
import SugarSyntax
import qualified Data.Set as S
import Control.Monad.Except

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
    ExceptT String IO InProgram  -- The loaded program
loadProg dir fileBaseName macroStack =
    if fileBaseName `elem` macroStack then
        throwError "Recursive macros detected."
    else do
        fileStr <- lift $ readFile $
            dir ++ pathSeparator : fileBaseName ++ ".while"
        let fileTokens  = scan fileStr fileBaseName
        suProg <- parseProg fileTokens
        case (suProg, macroStack) of
            ( SuProgram n _ _ _ , _  ) | nameName n /= fileBaseName ->
                throwError $ "Program name (" ++ nameName n
                    ++ ") must match file base name."
            ( _                 , [] ) ->
                desugarProg dir ( fileBaseName : macroStack ) suProg
            ( SuProgram n r b w , _  ) ->
                let namesToInit = S.delete r $ S.insert w $ namesSuBlock b
                    initCode    = map ( \n ->
                            SuAssign ( Info ( "+IMPL+", 0 ) ) n ( Lit ENil ) )
                        (S.toList namesToInit)
                in desugarProg dir ( fileBaseName : macroStack )
                    ( SuProgram n r ( initCode ++ b ) w )

-- Desugar a program, that is, convert it to pure while syntax
desugarProg :: FilePath -> [FilePath] -> SuProgram ->
    ExceptT String IO InProgram
desugarProg dir macroStack ( SuProgram n r blk w ) = do
    desugaredBlk <- desugarBlock dir macroStack blk
    return $ InProgram n r desugaredBlk w

-- Desugar a block
desugarBlock :: FilePath -> [FilePath] -> SuBlock -> ExceptT String IO InBlock
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
    ExceptT String IO InBlock
desugarComm dir macroStack suComm = case suComm of
    SuAssign i x exp -> return [ InAssign i x exp ]
    SuWhile i gd blk -> do
        desugaredBlk <- desugarBlock dir macroStack blk
        return [ InWhile i gd desugaredBlk ]
    SuIfElse i gd bt bf -> do
        desugaredBT <- desugarBlock dir macroStack bt
        desugaredBF <- desugarBlock dir macroStack bf
        return [ InIfElse i gd desugaredBT desugaredBF ]
    Macro i x f e -> do
        prog <- loadProg dir f macroStack
        return $
            [ InAssign i ( inReadVar prog ) e ] ++
            inBlock prog ++
            [ InAssign i x ( Var ( inWriteVar prog ) ) ]
    Switch i e cases def -> do
        desugaredDef   <- desugarBlock dir macroStack def
        desugaredCases <- sequence $ map ( \( matchE , blk ) -> do
                desugaredBlk <- desugarBlock dir macroStack blk
                return ( matchE , desugaredBlk )
            ) cases
        return $ [ InSwitch i e desugaredCases desugaredDef ]
