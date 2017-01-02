module Parsing.AST ( Node(..),
             parse
           )
    where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Parsing.SExpression as SExp
import Result

type Dict = Map String

data Node = Unit
          | Identifier String
          | Symbol String
          | Let {bindings :: Dict Node, body :: Node}
          | Letrec {bindings :: Dict Node, body :: Node}
          | Object (Dict Node)
          | Match {variable :: Node, branches :: (Dict Node)}
          | Function {parameterName :: String, body :: Node}
          | Application {function :: Node, argument :: Node}
          | Choose [Node]
          deriving (Show, Eq, Ord)

parse :: SExp.Node -> Result Node

parse (SExp.Atom ('\'':symbol)) = pure $ Symbol symbol
parse (SExp.Atom name) = pure $ Identifier name
parse (SExp.List []) = pure Unit

parse (SExp.List ((SExp.Atom "choose"):rest)) =
    Choose <$> sequenceA (map parse rest)

parse (SExp.List ((SExp.Atom "object"):rest)) = Object <$> parseMap rest
parse (SExp.List ((SExp.Atom "match"):variable:rest)) =
    Match <$> parse variable <*> parseMap rest

parse (SExp.List [firstWord, rest]) = Application <$> parse firstWord <*> parse rest

parse (SExp.List [SExp.Atom "lambda", SExp.Atom paramName, body]) =
    Function paramName <$> parse body

parse (SExp.List [firstWord, (SExp.List second), third]) =
    case firstWord of
      SExp.Atom "let" -> Let <$> parseMap second <*> parse third
      SExp.Atom "letrec" -> Letrec <$> parseMap second <*> parse third
      _ -> err "Incorrect syntax for list of three values."

parse _ = err "Incorrect syntax in general"
      
parseMap :: [SExp.Node] -> Result (Dict Node)
parseMap list = foldl (amLift2 addPairToMap) (pure Map.empty) (map parsePair list)

parsePair :: SExp.Node -> Result (String, Node)
parsePair (SExp.List [SExp.Atom name, rest]) = do
    parsedContent <- parse(rest)
    return (dropOptionalTick name, parsedContent)

parsePair (SExp.List [SExp.List [SExp.Atom name, SExp.Atom paramName], rest]) = do
    parsedContent <- parse(rest)
    return (dropOptionalTick name, Function paramName parsedContent)

parsePair _ = err "A pair isn't correct."

dropOptionalTick :: String -> String
dropOptionalTick = dropWhile (=='\'')

addPairToMap :: Dict Node -> (String, Node) -> Result (Dict Node)
addPairToMap dict (str, node) = if Map.notMember str dict
                                   then pure $ Map.insert str node dict
                                   else err $ "Key '"++str++"' defined multiple times"

