module Sexpression ( Node,
                     parse,
                   )
    where

import Data.Char

data Node = List [Node] | Atom String
    deriving (Show, Eq, Ord)

skipWhile f [] = []
skipWhile f (car:cdr) = if f car then skipWhile f cdr else car:cdr

separateWhile f a = (takeWhile f a, skipWhile f a)

removeBeginingSpace = skipWhile isSpace

parseAtom :: String -> (String, String)
parseAtom = separateWhile (\c -> not (isSpace c) && c /= ')' && c /= '(')

parseList :: String -> ([Node], String)
parseList [] = ([], "")
parseList (')':cdr) = ([], cdr)
parseList exp = (node:otherNodes , restOfExpression)
    where (otherNodes, restOfExpression) = parseList (removeBeginingSpace remainderFromFirstNode)
          (node, remainderFromFirstNode) = parseNode exp

parseNode :: String -> (Node, String)
parseNode ('(':cdr) = (List nodes, restOfExpression)
    where (nodes, restOfExpression) = parseList (removeBeginingSpace cdr)

parseNode exp = (Atom atom, restOfExpression)
    where (atom, restOfExpression) = parseAtom exp

          
parse :: String -> Node
parse = fst.parseNode.removeBeginingSpace

parseSexpressionList :: String -> Node
parseSexpressionList = (List).fst.parseList.removeBeginingSpace
