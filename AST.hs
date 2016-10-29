module AST ( Node,
             parse
           )
    where

import Data.Map (Map)
import qualified Sexpression as Sex
import Result

type Dict = Map String

data Node = Unit
          | Identifier String
          | Let {bindings :: Dict Node, body :: Node}
          | Letrec {bindings :: Dict Node, body :: Node}
          | Object (Dict Node)
          | Lookup {property :: String, object :: Node}
          | Enum {tag :: String, content :: Node}
          | Match (Dict Node)
          | Function {parameterName :: String, body :: Node}
          | Application {function :: Node, argument :: Node}
          | Choose [Node]
          deriving (Show, Eq, Ord)

parse :: Sex.Node -> Result Node

parse Sex.Atom ('\'':tag) -> pure Enum {tag=tag, content=Unit}
parse Sex.Atom name -> pure Identifier name
parse Sex.List [] -> pure Unit

parse Sex.List ((Sex.Atom "choose"):rest) =
    Choose $ sequenceA $ map parse rest

parse Sex.List [firstWord, rest] =
    case firstWord of
      Sex.Atom "object" -> Object <$> parseMap rest
      Sex.Atom "match" -> Match <$> parseMap rest
      Sex.Atom ('\'':tag) -> do
          parsedContent <- parse rest
          return Enum {tag=tag, content=parsedContent}
      Sex.Atom (':':tag) -> do
          parsedContent <- parse rest
          return Lookup {property=tag, object=parsedContent}
      firstWord -> Application <$> parse firstWord <*> parse rest

parse Sex.List [firstWord, second, third] =
    case firstWord of
      Sex.Atom "let" -> Let <$> parseMap second <*> parse third
      Sex.Atom "letrec" -> Letrec <$> parseMap second <*> parse third
      _ -> err "Incorrect syntax for list of three values."

parse _ -> "Incorrect syntax in general"

      
parseMap :: Sex.Node -> Result (Dict Node)
parseMap Sex.List list = foldl (amLift2 addPairToMap) (pure Map.empty) (map parsePair list)

parsePair :: Sex.Node -> Result (String, Node)
parsePair Sex.List [Sex.Atom name, rest] = do
    parsedContent <- parseAST(rest)
    return (name, parsedContent)
processPair _ = error "A pair isn't correct."

addPairToMap :: Dict Node -> (String, Node) -> Result (Dict Node)
addPairToMap dict (str, node) = if Map.notMember str dict
                                   then pure $ Map.insert key value map
                                   else err $ "Key '"++str++"' defined multiple times"

