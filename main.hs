import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.Lazy as Map.Lazy
import qualified Debug.Trace

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

ok a = Result $ Right a
err e = Result $ Left [e]


newtype FunctionType = FunctionType (Type -> Result String Type)

instance Show FunctionType where
    show f = "[function]"

data Type = Unit | Object (Map String Type) | Enum (Map String Type) | Function FunctionType | Any
    deriving Show


{-- In the current type-checking system, we need to know the types of all values always.
 Because of this, functions are only type-checked when they are called.
--}


mergeExpTypes :: Type -> Type -> Result String Type
mergeExpTypes Unit _ = pure Unit
mergeExpTypes _ Unit = pure Unit
mergeExpTypes Any other = pure other
mergeExpTypes other Any = pure other
mergeExpTypes (Object a) (Object b) = Object <$> intersectMapsOfTypesWith mergeExpTypes a b
mergeExpTypes (Enum a) (Enum b) = Enum <$> uniteMapsOfTypesWith mergeExpTypes a b
mergeExpTypes (Function (FunctionType a)) (Function (FunctionType b)) = (pure . Function . FunctionType) $ (\t -> ((mergeExpTypes <$> a t) =<<* b t))
mergeExpTypes a b = err $ "Cannot merge expression types '" ++ show a ++ "' and '" ++ show b ++ "'.\n"


mergeReqTypes :: Type -> Type -> Result String Type
mergeReqTypes Unit Unit = pure Unit
mergeReqTypes (Object a) (Object b) = Object <$> uniteMapsOfTypesWith mergeReqTypes a b
mergeReqTypes (Enum a) (Enum b) = Enum <$> intersectMapsOfTypesWith mergeReqTypes a b 
-- mergeReqTypes (Function aIn aOut) (Function bIn bOut) = Function <$> (mergeExpTypes aIn bIn) <*> (mergeReqTypes aOut bOut)
mergeReqTypes a b = err $ "Cannot merge required types '" ++ show a ++ "' and '" ++ show b ++ "'.\n"


intersectMapsOfTypesWith :: (Type -> Type -> Result String Type) -> Map String Type -> Map String Type -> Result String (Map String Type)
intersectMapsOfTypesWith f a b = sequenceA $ Map.intersectionWith f a b

uniteMapsOfTypesWith :: (Type -> Type -> Result String Type) -> Map String Type -> Map String Type -> Result String (Map String Type)
uniteMapsOfTypesWith f a b = sequenceA ( Map.unionWith (\(Result (Right a)) -> \(Result (Right b)) -> f a b) (fmap pure a) (fmap pure b))



data ValidatorState = ValidatorState (Map String Type) SexNode
    deriving Show

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
          List [(Atom "let"   ),List definitions,body] -> let additionalScope = getObjectType scope definitions
                                                              fullScope = (\as -> Map.Lazy.union as scope) `fmap` additionalScope
                                                              getTypeOfBody fs = getType (ValidatorState fs body)
                                                           in getTypeOfBody =<<< fullScope

          List [(Atom "->"), object, (Atom property)] -> lookupProperty =<<< getTypeConsideringScope object
              where lookupProperty (Object map) = case Map.lookup property map of
                                                    Just t -> ok t
                                                    Nothing -> err $ "The object "++ show (Object map)++" has no property '"++ property ++"'."
                    lookupProperty other = err $ "'"++ show other++"' isn't an object."

          List ((Atom "match"): enum: branches) -> match `fmap` getTypeConsideringScope enum =<<* branchMap
              where processBranch (List [(List [(Atom ('\'':token)), (Atom name)]), body]) = pure (token, (name, body))
                    processBranch other = err $ "Invalid syntax in branch '"++show other++"'."
                    branchMap = Map.fromList `fmap` sequenceA (map processBranch branches)
                    match (Enum enum) branches = if Map.isSubmapOfBy (\_-> \_->True) enum branches
                                                    then (Map.fold mergeBranchTypes (pure Any)) =<<< (sequenceA ( Map.intersectionWith getBranchType enum branches))
                                                    else err ( "Enum "++show enum++" isn't a submap of match branches "++show branches++".")

                    mergeBranchTypes a b = (mergeExpTypes a) =<<< b
                    getBranchType :: Type -> (String, SexNode) -> Result String Type
                    getBranchType enumValue (name, body) = getType (ValidatorState (Map.insert name enumValue scope) body)

          List [(Atom "lambda"), (Atom parameterName), body] -> (pure . Function . FunctionType) (\t ->
              Debug.Trace.trace ("evaluated the body of lambda "++show body) (getType (ValidatorState (Map.insert parameterName t scope) body)))

          List [function, parameter] -> let funcType = Debug.Trace.trace ("got funcType " ++ show function) (getTypeConsideringScope function)
                                            paramType = Debug.Trace.trace ("got paramType " ++ show parameter) (getTypeConsideringScope parameter)
                                         in Debug.Trace.trace ("evaluated function") ((Debug.Trace.trace "first part of func" (getFunc =<<< funcType)) =<<* paramType)
                                        where getFunc (Function (FunctionType func)) = pure func
                                              getFunc notFunction = err $ "'" ++ show notFunction ++ "' is not a function."

          Atom name -> case (Debug.Trace.trace ("lookuped value of "++name) (Map.lookup name scope)) of
                         Just t -> ok t
                         Nothing -> err $ "The variable '"++ name ++"' isn't defined."

          node -> err $ "invalid syntax: "++ show node


getObjectType :: (Map String Type) -> [SexNode] -> Result String (Map String Type)
getObjectType scope rest = foldl appendToObject (ok Map.empty) (map processPair rest)
    where processPair (List [Atom key, value]) = (\value -> (key,value)) <$> getType (ValidatorState scope value)
          processPair exp = err ("Incorrect sexpression '" ++ show exp ++ "' in object literal.")
          appendToObject :: Result String (Map String Type) -> Result String (String, Type) -> Result String (Map String Type)
          appendToObject map entry= appendUnwrappedToObject `fmap` entry =<<* map
              where appendUnwrappedToObject (key,value) map = if Map.notMember key map
                                                                 then ok $ Map.insert key value map
                                                                 else err $ "Key '"++ key ++"' defined multiple times"
      



rootGetType string = getType (ValidatorState Map.empty (parseSexpression string))


example' = rootGetType "(let ((y (lambda f ((lambda x (f (x x))) (lambda x (f (x x))))))) (y 'a))"
example'' = rootGetType "((lambda f ((lambda x (f (x x))) (lambda x (f (x x))))) 'a)"
example = rootGetType "(let ((y (lambda f ((lambda x (f (x x))) (lambda z (f (z z))))))) (y (lambda _ ())))"

exampleCorrect = case example of
                   (Result (Right _)) -> True
                   _ -> False

main = do putStrLn (show example)
