module Desugar ( loadProg , desugarProg ) where

import System.FilePath (pathSeparator)
import Parser (parseProg)
import Lexer (scan)
import PureSyntax
import SugarSyntax

-- Given its directory, base name, a macro call stack and macro seed, load a
-- program from disk, returning it with the macro seed after unparsing etc.
-- Also check for macro recursion or non-matching file+prog names, failing if
-- either is found.
loadProg :: FilePath -> FilePath -> [FilePath] -> Int -> IO (Int, Program)
loadProg dir fileBaseName macroStack macroNum =
    if fileBaseName `elem` macroStack then
        error "Recursive macros detected."
    else do
        fileStr <- readFile $ dir ++ pathSeparator : fileBaseName ++ ".while"
        let fileTokens = scan fileStr ( fileBaseName ++ "+" ++ show macroNum )
        let suProg     = parseProg fileTokens
        case suProg of
            SuProgram n _ _ _ | nameName n /= fileBaseName -> error $
                "Program name (" ++ nameName n ++ ") must match file base name."
            _ -> desugarProg dir ( fileBaseName : macroStack ) macroNum suProg

-- Desugar a program, that is, convert it to pure while syntax
desugarProg :: FilePath -> [FilePath] -> Int -> SuProgram -> IO (Int, Program)
desugarProg dir macroStack macroNum ( SuProgram n r blk w ) = do
    ( macroNum' , desugaredBlk ) <- desugarBlock dir macroStack macroNum blk
    return ( macroNum' , Program n r desugaredBlk w )

-- Desugar a block
desugarBlock :: FilePath -> [FilePath] -> Int -> SuBlock -> IO (Int, Block)
desugarBlock dir _              macroNum []         = return ( macroNum , [] )
desugarBlock dir macroStack macroNum ( c : cs ) = do
    ( macroNum'  , desugaredC  ) <- desugarComm  dir macroStack macroNum  c
    ( macroNum'' , desugaredCs ) <- desugarBlock dir macroStack macroNum' cs
    return ( macroNum'' , desugaredC ++ desugaredCs )

-- Desugar a command
desugarComm ::
    FilePath   -> -- Path to search for macro files
    [FilePath] -> -- Macro call stack
    Int        -> -- Seed for names
    SuCommand  -> -- The command to desugar
    IO (Int, Block)
desugarComm dir macroStack macroNum suComm = case suComm of
    SuAssign x exp -> return ( macroNum, [ Assign x exp ] )
    SuWhile gd blk -> do
        ( macroNum' , desugaredBlk ) <- desugarBlock dir macroStack macroNum blk
        return ( macroNum , [ While gd desugaredBlk ] )
    SuIfElse gd bt bf -> do
        ( macroNum'  , desugaredBT ) <- desugarBlock dir macroStack macroNum  bt
        ( macroNum'' , desugaredBF ) <- desugarBlock dir macroStack macroNum' bf
        return ( macroNum'' , [ IfElse gd desugaredBT desugaredBF ] )
    Macro x f e -> do
        ( macroNum' , prog ) <- loadProg dir f macroStack ( succ macroNum )
        return ( macroNum' ,
            [ Assign ( readVar prog ) e ] ++
            block prog ++
            [ Assign x ( Var ( writeVar prog ) ) ] )
    Switch e [] def -> desugarBlock dir macroStack macroNum def
    Switch e ( ( matchE , blk ) : cases ) def -> do
        ( macroNum'  , desugaredBlk  ) <-
            desugarBlock dir macroStack macroNum blk
        ( macroNum'' , desugaredRest ) <-
            desugarComm  dir macroStack macroNum' ( Switch e cases def )
        return ( macroNum'' ,
            [ IfElse ( IsEq e matchE ) desugaredBlk desugaredRest ] )
