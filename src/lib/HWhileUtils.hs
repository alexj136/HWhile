module HWhileUtils where

import qualified Data.Map        as M
import qualified Data.Set        as S
import System.FilePath
import qualified Lexer           as L
import qualified Parser          as P
import qualified PureSyntax      as PS
import SugarSyntax
import qualified PureInterpreter as I
import qualified Unparser        as U

pathAndTextToProg :: FilePath -> String -> SuProgram
pathAndTextToProg fp = P.parseProg . L.scan fp

pathAndTextToComm :: FilePath -> String -> SuCommand
pathAndTextToComm fp = P.parseComm . L.scan fp

pathAndTextToExpr :: FilePath -> String -> PS.Expression
pathAndTextToExpr fp = P.parseExpr . L.scan fp

pathAndTextToVal  :: FilePath -> String -> PS.ETree
pathAndTextToVal  fp = P.parseLVal . L.scan fp

-- Run a program given the file path, and a map from file paths to files that
-- are in the macro tree for the given file path
runFromParts ::
    FilePath                    ->  -- The 'main' file
    (M.Map FilePath SuProgram)  ->  -- The map from filenames to programs
    String                      ->  -- The argument string
    PS.ETree                        -- The result of the execution
runFromParts mainFile fileMap argStr =
    I.evalProg (pathAndTextToVal "+IMPL+" argStr)
        (desugarProg fileMap (fileMap M.! mainFile))

-- Open all files needed. Call 'buildFileMap M.empty (S.singleton filePath) to
-- open the file at 'filePath', and all files that the program in 'filePath'
-- uses, and all files that those files use, etc. Fails if there are circular
-- dependencies. Returns results in a map from file paths to the corresponding
-- programs. These can be passed to SugarSyntax.desugarProg.
buildFileMap ::
    FilePath                                   -> -- The search path
    M.Map FilePath (S.Set FilePath, SuProgram) ->
    S.Set FilePath                             ->
    IO (M.Map FilePath SuProgram)
buildFileMap directory ingraph tovisit =
    if S.null tovisit then
        return $ M.map snd ingraph
    else do
        let curFileBaseName = S.findMin tovisit
        let tovisitRest     = S.deleteMin tovisit
        curFileText <- readFile (fullPath directory curFileBaseName)
        let curSuProg   = pathAndTextToProg curFileBaseName curFileText
        let curChildren = macroNamesProg curSuProg
        let newIngraph  = M.insert curFileBaseName
                (curChildren, curSuProg) ingraph
        let newTovisit  = S.union tovisitRest curChildren
        if (not . S.null) (S.intersection curChildren (M.keysSet newIngraph))
        then
            error "Recursive macros found. Macros may not be recursive."
        else
            buildFileMap directory newIngraph newTovisit

fullPath :: FilePath -> FilePath -> FilePath
fullPath directory baseName =
    directory ++ [pathSeparator] ++ baseName ++ ".while"
