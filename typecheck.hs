import System.IO
import System.Environment

import qualified Sexpression as Sex
import Result
import qualified AST
import qualified Var


process s = Var.actualEval =<<( AST.parse $ Sex.parse $ s)

main :: IO ()
main = do
    args <- getArgs
    contents <- if args==[] then getContents else readFile (head args)
    putStrLn $ show $ process contents
