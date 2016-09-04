import Data.Char
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map

data SexNode = List [SexNode] | Atom String
    deriving Show

skipWhile f [] = []
skipWhile f (car:cdr) = if f car then skipWhile f cdr else car:cdr

separateWhile f a = (takeWhile f a, skipWhile f a)

removeBeginingSpace = skipWhile isSpace

parseAtom :: String -> (String, String)
parseAtom = separateWhile (\c -> not (isSpace c) && c /= ')' && c /= '(')

parseList :: String -> ([SexNode], String)
parseList [] = ([], "")
parseList (')':cdr) = ([], cdr)
parseList exp = (node:otherNodes , restOfExpression)
    where (otherNodes, restOfExpression) = parseList (removeBeginingSpace remainderFromFirstNode)
          (node, remainderFromFirstNode) = parseNode exp

parseNode :: String -> (SexNode, String)
parseNode ('(':cdr) = (List nodes, restOfExpression)
    where (nodes, restOfExpression) = parseList (removeBeginingSpace cdr)

parseNode exp = (Atom atom, restOfExpression)
    where (atom, restOfExpression) = parseAtom exp

          
parseSexpression :: String -> SexNode
parseSexpression = fst.parseNode.removeBeginingSpace

parseSexpressionList :: String -> SexNode
parseSexpressionList = (List).fst.parseList.removeBeginingSpace




newtype Result e a = Result { runResult :: Either [e] a }
    deriving Show

instance Functor (Result e) where
    fmap f (Result (Right a)) = Result $ Right $ f a
    fmap _ (Result (Left es)) = Result $ Left es

instance Applicative (Result e) where
    pure a = Result $ Right a
    (Result f) <*> (Result a) = Result $
        case (f, a) of
          (Right f', Right a') -> Right $ f' a'
          (Left fes, Left aes) -> Left $ fes ++ aes
          (Left es, _) -> Left es
          (_, Left es) -> Left es



ok a = Result $ Right a
err e = Result $ Left [e]


data Type = Unit | Object (Map String Type) | Enum (Map String Type) | Function Type Type
    deriving Show

{-- There is two kinds of types when type checking "expression types" and "required types".
    Expression are the types that are usually known, like when creating an object, you know what its properties are.
    Required types are the types that are the inputs of functions.
--}


mergeExpTypes :: Type -> Type -> Result String Type
mergeExpTypes Unit Unit = pure Unit
mergeExpTypes (Object a) (Object b) = Object <$> intersectMapsOfTypesWith mergeExpTypes a b
mergeExpTypes (Enum a) (Enum b) = Enum <$> uniteMapsOfTypesWith mergeExpTypes a b
mergeExpTypes (Function aIn aOut) (Function bIn bOut) = Function <$> (mergeReqTypes aIn bIn) <*> (mergeExpTypes aOut bOut)
mergeExpTypes a b = err $ "Cannot merge expression types '" ++ show a ++ "' and '" ++ show b ++ "'.\n"


mergeReqTypes :: Type -> Type -> Result String Type
mergeReqTypes Unit Unit = pure Unit
mergeReqTypes (Object a) (Object b) = Object <$> uniteMapsOfTypesWith mergeReqTypes a b
mergeReqTypes (Enum a) (Enum b) = Enum <$> intersectMapsOfTypesWith mergeReqTypes a b 
mergeReqTypes (Function aIn aOut) (Function bIn bOut) = Function <$> (mergeExpTypes aIn bIn) <*> (mergeReqTypes aOut bOut)
mergeReqTypes a b = err $ "Cannot merge required types '" ++ show a ++ "' and '" ++ show b ++ "'.\n"


intersectMapsOfTypesWith :: (Type -> Type -> Result String Type) -> Map String Type -> Map String Type -> Result String (Map String Type)
intersectMapsOfTypesWith f a b = sequenceA $ Map.intersectionWith f a b

uniteMapsOfTypesWith :: (Type -> Type -> Result String Type) -> Map String Type -> Map String Type -> Result String (Map String Type)
uniteMapsOfTypesWith f a b = sequenceA ( Map.unionWith (\(Result (Right a)) -> \(Result (Right b)) -> f a b) (fmap pure a) (fmap pure b))



data ValidatorState = ValidatorState (String -> Maybe Type) SexNode

getType :: ValidatorState -> Result String Type
getType (ValidatorState scope sexpression) =
    let getTypeConsideringScope value = getType (ValidatorState scope value)
     in case sexpression of
          List [] -> pure Unit
          List ((Atom "object"):rest) -> Object `fmap` getObjectType scope rest
          Atom ('\'':token) -> pure $ Enum $ Map.fromList [(token,Unit)]
          List [Atom ('\'':token), value] -> (\value -> Enum $ Map.fromList [(token,value)]) <$> getTypeConsideringScope value
          -- choose simulates what will happen to types during conditionals
          List [Atom "choose", value1, value2] -> mergeExpTypes `fmap` getTypeConsideringScope value1 =<<* getTypeConsideringScope value2
          List [(Atom "letrec"),List definitions,body] -> let additionalScope = (\fs -> getObjectType fs definitions) fullScopeFunction
                                                              fullScopeFunction key =
                                                                  case additionalScope of
                                                                    Result (Right as) -> if Map.member key as
                                                                                            then Just (as Map.! key)
                                                                                            else scope key
                                                                    _ -> Nothing
                                                              getTypeOfBody fs = getType (ValidatorState fs body)
                                                           in getTypeOfBody fullScopeFunction
          node -> err $ "invalid syntax: "++ show node


(Result f) =<<* (Result a) =
    case (f, a) of
      (Right f', Right a') -> f' a'
      (Left fes, Left aes) -> Result $ Left $ fes ++ aes
      (Left es, _) -> Result $ Left es
      (_, Left es) -> Result $ Left es

f =<<< (Result a) =
    case a of
      Right a' -> f a'
      Left e -> Result $ Left $ e



getObjectType :: (String -> Maybe Type) -> [SexNode] -> Result String (Map String Type)
getObjectType scope rest = foldl appendToObject (ok Map.empty) (map processPair rest)
    where processPair (List [Atom key, value]) = (\value -> (key,value)) <$> getType (ValidatorState scope value)
          processPair exp = err ("Incorrect sexpression '" ++ show exp ++ "' in object literal.")
          appendToObject :: Result String (Map String Type) -> Result String (String, Type) -> Result String (Map String Type)
          appendToObject map entry= appendUnwrappedToObject `fmap` entry =<<* map
              where appendUnwrappedToObject (key,value) map = if Map.notMember key map
                                                                 then ok $ Map.insert key value map
                                                                 else err $ "Key '"++ key ++"' defined multiple times"
      



rootGetType string = getType (ValidatorState (\_ -> Nothing) (parseSexpression string))



main = do putStrLn (show (rootGetType "(letrec ((a ())) ()"))
