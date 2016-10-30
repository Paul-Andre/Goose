import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.Lazy as Map.Lazy
import qualified Debug.Trace

import qualified Sexpression as Sex
import Result
import qualified AST
import qualified Var

type Dict = Map String

example = "(letrec (((a b) (b b)) (b a)) a)"
example'' = Var.actualEval =<<( AST.parse $ Sex.parse $ example)

main = do putStrLn (show example'')
