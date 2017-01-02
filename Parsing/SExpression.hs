module Parsing.SExpression ( Node(List,Atom),
                     parse,
                   )
    where

import Text.ParserCombinators.Parsec as Parsec
import Data.Char

data Node = List [Node] | Atom String
    deriving (Show, Eq, Ord)


parser = do
    c <- anyChar
    pure Atom([c])

{--
listParser = do
    char '('
    ret <- sepBy parser (many1 (satisfy isSpace))
    char ')'
    return (List ret)

atomParser = fmap Atom $ many (noneOf (satisfy isSpace))
--}

parse input = Parsec.parse parser "{u}" input

--parseSExpressionList :: String -> Node
--parseSExpressionList = (List).fst.parseList.removeBeginingSpace
