module Result
    where

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

instance Monad (Result) where
    return a = Result $ Right a
    (Result a) >>= f =
        case a of
          Right a' -> f a'
          Left es -> Result $ Left es
    fail e = Result $ Left [e]

err e = Result $ Left [e]

amLift2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
amLift2 f a b = ((,) <$> a <*> b) >>= \(a',b') -> f a' b'
