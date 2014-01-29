import qualified Lexer as L
import qualified Parser as P
import qualified Syntax as S
import qualified Interpreter as I

readProg :: String -> S.Program
readProg = P.parseProg . L.alexScanTokens 

readComm :: String -> S.Command
readComm = P.parseComm . L.alexScanTokens 

readExpr :: String -> S.Expression
readExpr = P.parseExpr . L.alexScanTokens 
