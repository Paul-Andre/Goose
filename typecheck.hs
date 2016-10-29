import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.Lazy as Map.Lazy
import qualified Debug.Trace

import qualified Sexpression as Sex
import Result
import qualified AST
import qualified Var

type Dict = Map String

--example' = rootGetType "(let ((y (lambda f ((lambda x (f (x x))) (lambda x (f (x x))))))) (y 'a))"
example = "(object (a ()))"
example'' = Var.actualEval =<<( AST.parse $ Sex.parse $ example)


main = do putStrLn (show example'')
