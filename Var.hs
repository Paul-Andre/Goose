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

eval (AST.Function paramName body) = do
    env <- Reader.ask
    pure $ pure $Function { environment=env, paramName=paramName, body=body }

eval (AST.Application func arg) = do
    func' <- eval func
    arg' <- eval arg
    pure $ (amLift2 evalApplication) func' arg'

eval (AST.Choose _) = pure $ err $"Choose isn't meant to be evaluated"
eval node = pure $ err $"Evaluation of "++(show node)++" isn't implemented yet"


evalApplication :: Var -> Var -> Result Var
evalApplication (TaggedVar tv) v = pure $ TaggedVar $ Tagged (tv) v 
evalApplication (Object o) (TaggedVar (Symbol s)) =
    case Map.lookup s o of
      Just(v) -> pure v
      Nothing -> err $ "Object "++(show o)++" doesn't contain \""++s++"\"."

evalApplication (Object o) (TaggedVar (Tagged tag var)) = do
    firstApplication <- evalApplication (Object o) (TaggedVar tag)
    evalApplication firstApplication var

evalApplication (Function env pn b) v =
    let newEnv = Map.insert pn v env
     in Reader.runReader (eval b) newEnv

evalApplication a b = err $ "Cannot apply "++(show a)++" on "++(show b)++"."


evalMap :: (Dict AST.Node) -> Reader.Reader (Dict Var) (Result (Dict Var))
evalMap map = (fmap sequenceA $ sequenceA $ fmap eval map)

actualEval :: AST.Node -> Result Var
actualEval node  = Reader.runReader (eval node) Map.empty

