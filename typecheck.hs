import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.Lazy as Map.Lazy
import qualified Debug.Trace



data SexNode = List [SexNode] | Atom String
    deriving (Show, Eq, Ord)

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




newtype Result a = Result { runResult :: Either [String] a }
    deriving (Show, Eq, Ord)

instance Functor (Result) where
    fmap f (Result (Right a)) = Result $ Right $ f a
    fmap _ (Result (Left es)) = Result $ Left es

instance Applicative (Result) where
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



type Dict = Map String


data FunctionType = FunctionType String SexNode (Dict Type)
    deriving (Show, Eq, Ord)

data Type = Unit | Object (Dict Type) | Enum (Dict Type) | Function [FunctionType] | Incomplete (Result Type) Type | Any
    deriving (Show, Eq, Ord)

mergeExpTypes :: Type -> Type -> Result Type
mergeExpTypes Unit _ = pure Unit
mergeExpTypes _ Unit = pure Unit
mergeExpTypes Any other = pure other
mergeExpTypes other Any = pure other
mergeExpTypes (Object a) (Object b) = Object <$> intersectMapsOfTypesWith mergeExpTypes a b
mergeExpTypes (Enum a) (Enum b) = Enum <$> uniteMapsOfTypesWith mergeExpTypes a b
mergeExpTypes (Function a) (Function b) = pure (Function (a++b))
mergeExpTypes (Incomplete unknown known) other = (Incomplete unknown) <$> mergeExpTypes known other
mergeExpTypes other (Incomplete unknown known)= (Incomplete unknown) <$> mergeExpTypes other known
mergeExpTypes a b = err $ "Cannot merge expression types '" ++ show a ++ "' and '" ++ show b ++ "'.\n"


intersectMapsOfTypesWith :: (Type -> Type -> Result Type) -> Dict Type -> Dict Type -> Result (Dict Type)
intersectMapsOfTypesWith f a b = sequenceA $ Map.intersectionWith f a b

uniteMapsOfTypesWith :: (Type -> Type -> Result Type) -> Dict Type -> Dict Type -> Result (Dict Type)
uniteMapsOfTypesWith f a b = sequenceA ( Map.unionWith (\(Result (Right a)) -> \(Result (Right b)) -> f a b) (fmap pure a) (fmap pure b))


data ValidatorState = ValidatorState (Dict Type) (Map ([FunctionType], Type) Type) SexNode
    deriving (Show, Eq, Ord)

callWithType :: [FunctionType] -> Type -> (Map ([FunctionType],Type) Type) -> Result Type
callWithType functions inType previouslyCalled = if Map.member (functions,inType) previouslyCalled
    then case previouslyCalled Map.! (functions,inType) of
        Incomplete unknown known -> pure known
        theType -> pure theType
    else theType
        where theType = foldl (\a -> \b -> ((mergeExpTypes <$> a) =<<* b)) (pure Any) mapped 
                  where mapped = (map evaluate functions)
                        evaluate (FunctionType paramName body scope) = getType state 
                            where state = ValidatorState (Map.insert paramName inType scope) (Map.insert (functions,inType) (Incomplete theType Any) previouslyCalled) body

getType :: ValidatorState -> Result Type
getType (ValidatorState scope calledFunctions sexpression) =
    let getTypeConsideringScope value = getType (ValidatorState scope calledFunctions value)
        getObjectType :: [SexNode] -> Result (Dict Type)
        getObjectType rest = foldl appendToObject (ok Map.empty) (map processPair rest)
            where processPair (List [Atom key, value]) = (\value -> (key,value)) <$> getTypeConsideringScope value
                  processPair exp = err ("Incorrect sexpression '" ++ show exp ++ "' in object literal.")
                  appendToObject :: Result (Dict Type) -> Result (String, Type) -> Result (Dict Type)
                  appendToObject map entry= appendUnwrappedToObject `fmap` entry =<<* map
                      where appendUnwrappedToObject (key,value) map = if Map.notMember key map
                                                                         then ok $ Map.insert key value map
                                                                         else err $ "Key '"++ key ++"' defined multiple times"
     in case sexpression of
          List [] -> pure Unit
          List ((Atom "object"):rest) -> Object `fmap` getObjectType rest
          Atom ('\'':token) -> pure $ Enum $ Map.fromList [(token,Unit)]
          List [Atom ('\'':token), value] -> (\value -> Enum $ Map.fromList [(token,value)]) <$> getTypeConsideringScope value
          -- choose simulates what will happen to types during conditionals
          List [Atom "choose", value1, value2] -> mergeExpTypes `fmap` getTypeConsideringScope value1 =<<* getTypeConsideringScope value2
          List [(Atom "let"   ),List definitions,body] -> let additionalScope = getObjectType definitions
                                                              fullScope = (\as -> Map.Lazy.union as scope) `fmap` additionalScope
                                                              getTypeOfBody fs = getType (ValidatorState fs calledFunctions body)
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
                    getBranchType :: Type -> (String, SexNode) -> Result Type
                    getBranchType enumValue (name, body) = getType (ValidatorState (Map.insert name enumValue scope) calledFunctions body)

          List [(Atom "lambda"), (Atom parameterName), body] -> pure.Function $ [(FunctionType parameterName body scope)]

          List [function, parameter] -> let funcType = getTypeConsideringScope function
                                            paramType = getTypeConsideringScope parameter
                                            getFunc (Function functions) = pure (\paramType -> callWithType functions paramType calledFunctions)
                                            getFunc notFunction = err $ "'" ++ show notFunction ++ "' is not a function."
                                         in Debug.Trace.trace ("evaluated function ("++show function++ "  "++show parameter++")" ) ((getFunc =<<< funcType) =<<* paramType )

          Atom name -> case (Debug.Trace.trace ("lookuped value of "++name) (Map.lookup name scope)) of
                         --Just (Incomplete t other) -> (\t -> (mergeExpTypes t other)) =<<< t
                         Just t -> ok t
                         Nothing -> err $ "The variable '"++ name ++"' isn't defined."

          node -> err $ "invalid syntax: "++ show node


      



rootGetType string = getType (ValidatorState Map.empty Map.empty (parseSexpression string))


example' = rootGetType "(let ((y (lambda f ((lambda x (f (x x))) (lambda x (f (x x))))))) (y 'a))"
example'' = rootGetType "((lambda f ((lambda x (f (x x))) (lambda x (f (x x))))) (lambda f (choose ('a f) 'b)))"
example = rootGetType "(let ((y (choose (lambda _ 'yes) (lambda _ 'no)))) y)"

exampleCorrect = case example of
                   (Result (Right _)) -> True
                   _ -> False

main = do putStrLn (show example'')
