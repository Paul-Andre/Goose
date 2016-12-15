import System.IO
import System.Environment

import qualified SExpression as SExp
import Result
import qualified AST
import qualified Eval


process s = Eval.actualEval =<<( AST.parse $ SExp.parse $ s)

main :: IO ()
main = do
    args <- getArgs
    contents <- if args==[] then getContents else readFile (head args)
    putStrLn $ show $ process contents
