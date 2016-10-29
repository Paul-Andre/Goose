import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.Lazy as Map.Lazy
import qualified Debug.Trace

import qualified Sexpression as Sex
import Result

type Dict = Map String


data FunctionType = FunctionType String SexNode (Dict Type)
    deriving (Show, Eq, Ord)

data Type = Unit
          | Object (Dict Type)
          | Enum (Dict Type)
          | Function [FunctionType]
          | Incomplete (Result Type) Type
          | Any
          | Base ([FunctionType], Type) Type -- base case of a certain function called with certain parameter
    deriving (Show, Eq, Ord)

mergeExpTypes :: Type -> Type -> Result Type
mergeExpTypes Unit _ = pure Unit
mergeExpTypes _ Unit = pure Unit
mergeExpTypes Any other = pure other
mergeExpTypes other Any = pure other
mergeExpTypes (Object a) (Object b) = Object <$> intersectMapsOfTypesWith mergeExpTypes a b
mergeExpTypes (Enum a) (Enum b) = Enum <$> uniteMapsOfTypesWith mergeExpTypes a b
mergeExpTypes (Function a) (Function b) = pure (Function (a++b))
mergeExpTypes (Incomplete unknownA knownA) (Incomplete unknownB knownB) = (Incomplete ((mergeExpTypes <$> unknownA) =<<* unknownB)) <$> mergeExpTypes knownA knownB
mergeExpTypes (Incomplete unknown known) other = (Incomplete unknown) <$> mergeExpTypes known other
mergeExpTypes other (Incomplete unknown known)= (Incomplete unknown) <$> mergeExpTypes other known
mergeExpTypes (Base a) (Base b) = Base <$> mergeExpTypes a b
mergeExpTypes (Base a) b = Base <$> mergeExpTypes a b
mergeExpTypes a (Base b) = Base <$> mergeExpTypes a b
mergeExpTypes a b = err $ "Cannot merge expression types '" ++ show a ++ "' and '" ++ show b ++ "'.\n"

intersectMapsOfTypesWith :: (Type -> Type -> Result Type) -> Dict Type -> Dict Type -> Result (Dict Type)
intersectMapsOfTypesWith f a b = sequenceA $ Map.intersectionWith f a b

uniteMapsOfTypesWith :: (Type -> Type -> Result Type) -> Dict Type -> Dict Type -> Result (Dict Type)
uniteMapsOfTypesWith f a b = sequenceA ( Map.unionWith (\(Result (Right a)) -> \(Result (Right b)) -> f a b) (fmap pure a) (fmap pure b))

-- This is used to get the "known" part of a type that might contain incomplete types
getKnown :: Type -> Type -> Type
getKnown (Incomplete unknown known) = getKnown known
getKnown (Object a) = Object (fmap getKnown a)
getKnown (Enum a) = Enum (fmap getKnown a)
getKnown other = other

data ValidatorState = ValidatorState (Dict Type) (Map ([FunctionType], Type) Type) SexNode
    deriving (Show, Eq, Ord)

callWithType :: [FunctionType] -> Type -> (Map ([FunctionType],Type) Type) -> Result Type
callWithType functions inType previouslyCalled =
    case Map.lookup (functions,inType) previouslyCalled of
      Just theType -> case getKnown theType of
                        Any -> err $ "Recursive function " ++ show functions ++ " isn't valid."
                        other -> pure (Base (functions,inType) other)

      Nothing -> let base = foldl (\a -> \b -> ((mergeExpTypes <$> a) =<<* b)) (final) mapped 
                     mapped = (map evaluateBase functions)
                     evaluateBase (FunctionType paramName body scope) = getType stateForBase
                     stateForBase = ValidatorState (Map.insert paramName inType scope) (Map.insert (functions,inType) (Incomplete base any) previouslyCalled) body
                  in \base -> case base of
                                Base (fs, inT) type ->
                                    | (fs, inT) == (functions, inType) -> 
                                        let result = 
              


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
