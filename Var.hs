module Var ( Var, eval, actualEval)
    where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Control.Monad.Reader as Reader

import qualified AST
import Result

import Data.Functor.Identity

type Dict = Map String

data TaggedVar = Symbol String | Tagged {tag :: TaggedVar, var :: Var}
        deriving (Show, Eq, Ord)

data Var = Unit
         | Object (Dict Var)
         | TaggedVar TaggedVar
         | Function {environment :: Dict Var,
                     paramName :: String,
                     body :: AST.Node
                    }
        deriving (Show, Eq, Ord)

eval :: AST.Node -> Reader.Reader (Dict Var) (Result Var)
eval AST.Unit = pure $ pure Unit
eval (AST.Identifier s) = do
    env <- Reader.ask
    case Map.lookup s env of
      Just(var) -> pure $ pure var
      Nothing -> pure $ err $ "Couldn't find variable \""++s++"\"."

eval (AST.Symbol s) = pure $ pure $ TaggedVar $ Symbol s

eval (AST.Let bindings body) = do
    env <- Reader.ask
    let newBindings = Reader.runReader (evalMap bindings) env
        newBindings :: Result (Dict Var)
     in 
        pure $ newBindings >>=
            \nb -> Reader.runReader (eval body) (Map.union nb env)

eval (AST.Object object) = do
    env <- Reader.ask
    let evaluatedObject = Object <$> Reader.runReader (evalMap object) env
     in 
        pure $ evaluatedObject

eval (AST.Choose _) = pure $ err $"Choose isn't meant to be evaluated"
eval node = pure $ err $"Evaluation of "++(show node)++" isn't implemented yet"

evalMap :: (Dict AST.Node) -> Reader.Reader (Dict Var) (Result (Dict Var))
evalMap map = (fmap sequenceA $ sequenceA $ fmap eval map)

actualEval :: AST.Node -> Result Var
actualEval node  = Reader.runReader (eval node) Map.empty

