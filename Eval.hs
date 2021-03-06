module Eval ( Value, eval, actualEval)
    where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Control.Monad.Reader as Reader

import qualified Parsing.AST as AST
import Result

type Dict = Map String

data TaggedValue = Symbol String | Tagged {tag :: TaggedValue, value :: Value}
        deriving (Show, Eq, Ord)

data Value = Unit
         | Object (Dict Value)
         | TaggedValue TaggedValue
         | Function {environment :: Dict Value,
                     paramName :: String,
                     body :: AST.Node
                    }
         | Dummy (Result Value)
        deriving (Show, Eq, Ord)


eval :: AST.Node -> Reader.Reader (Dict Value) (Result Value)

eval AST.Unit = pure $ pure Unit

eval (AST.Identifier s) = do
    env <- Reader.ask
    case Map.lookup s env of
      Just(Dummy dum) -> pure $ dum
      Just(value) -> pure $ pure $ value
      Nothing -> pure $ err $ "Couldn't find variable "++s++"."

eval (AST.Symbol s) = pure $ pure $ TaggedValue $ Symbol s

eval (AST.Let bindings body) = do
    env <- Reader.ask
    let newBindings = Reader.runReader (evalMap bindings) env
        newBindings :: Result (Dict Value)

     in 
        pure $ newBindings >>=
            \nb -> Reader.runReader (eval body) (Map.union nb env)

eval (AST.Letrec bindings body) = do
    env <- Reader.ask
    let newBindings = Reader.runReader (evalMapLoose bindings) (Map.union newBindings env)
     in 
        pure $ Reader.runReader (eval body) (Map.union newBindings env)

eval (AST.Object object) = do
    evaluatedObject <- evalMap object
    pure $ Object <$>  evaluatedObject

eval (AST.Function paramName body) = do
    env <- Reader.ask
    pure $ pure $ Function { environment=env, paramName=paramName, body=body }

eval (AST.Application func arg) = do
    func' <- eval func
    arg' <- eval arg
    pure $ (amLift2 evalApplication) func' arg'

eval (AST.Match var branches) = do
    func' <- eval var
    -- The branches can be evaluated since the way match works now, it's
    -- practically just sugar for creating an object containing functions and
    -- applying it to var.
    arg' <- evalMap branches
    pure $ (amLift2 evalApplication) (Object <$> arg') func'

eval (AST.Choose _) = pure $ err $"Choose isn't meant to be evaluated"


evalApplication :: Value -> Value -> Result Value
evalApplication (TaggedValue tv) v = pure $ TaggedValue $ Tagged (tv) v 
evalApplication (Object o) (TaggedValue (Symbol s)) =
    case Map.lookup s o of
      Just(v) -> pure v
      Nothing -> err $ "Object "++(show o)++" doesn't contain \""++s++"\"."

evalApplication (Object o) (TaggedValue (Tagged tag var)) = do
    firstApplication <- evalApplication (Object o) (TaggedValue tag)
    evalApplication firstApplication var

evalApplication (Function env pn b) v =
    let newEnv = Map.insert pn v env
     in Reader.runReader (eval b) newEnv

evalApplication a b = err $ "Cannot apply "++(show a)++" on "++(show b)++"."


evalMap :: (Dict AST.Node) -> Reader.Reader (Dict Value) (Result (Dict Value))
evalMap map = (fmap sequenceA $ sequenceA $ fmap eval map)

actualEval :: AST.Node -> Result Value
actualEval node  = Reader.runReader (eval node) Map.empty

evalMapLoose :: (Dict AST.Node) -> Reader.Reader (Dict Value) (Dict Value)
evalMapLoose map = (fmap (fmap Dummy)) $ sequenceA $ fmap eval map

