{-# LANGUAGE RankNTypes #-}

module Chapter6 where

-- applyToFive :: forall a. (a -> a) -> Int
-- applyToFive f = f 5
-- This does not work since it would allow any function (a -> a) to be given, i.e. (Bool -> Bool)
-- So we have to restrict the functions we can recieve, to say they must be able to take any 'a' and give that 'a'
nice :: forall a. (a -> a) -> Int
nice f = 69

-- >>> nice not
-- 69

applyToFive :: (forall a. a -> a) -> Int
applyToFive f = f 5

-- 6.3-i
-- Int -> forall a. a -> a
-- Int -> (forall a. (a -> a))
-- rank-2

-- 6.3-ii
-- (a -> b) -> (forall c. c -> a) -> b
-- forall a b. (a -> b) -> (forall c. c -> a) -> b
-- rank-2

-- 6.3-iii
-- ((forall x. m x -> b (z m x)) -> b (z m a)) -> m a
-- wtf.

-- `a` and `forall r. (a -> r) -> r` are isomorphic, very poggers.

cont :: a -> (forall r. (a -> r) -> r)
cont a = \callback -> callback a

runCont :: (forall r. (a -> r) -> r) -> a
runCont f =
  let callback = id
   in f callback

newtype Cont a = Cont
  { unCont :: forall r. (a -> r) -> r
  }

instance Functor Cont where
  fmap f (Cont cont) =
    -- Just like magic
    Cont $ \br -> cont $ \a -> (\b -> br b) $ f a

instance Applicative Cont where
  pure a = Cont $ \callback -> callback a
  (Cont cab) <*> (Cont cont) =
    Cont $ \br -> cont $ \a -> (\b -> br b) $ (\a -> cab id a) a

instance Monad Cont where
  (Cont a) >>= amb =
    amb $ a id

withVersionNumber :: (Double -> r) -> r
withVersionNumber f = f 1.0

withTimestamp :: (Int -> r) -> r
withTimestamp f = f 1532083362

withOS :: (String -> r) -> r
withOS f = f "linux"

releaseString :: String
releaseString =
  withVersionNumber $ \version ->
    withTimestamp $ \date ->
      withOS $ \os ->
        os ++ "-" ++ show version ++ "-" ++ show date

releaseStringCont :: String
releaseStringCont =  runCont $ unCont $ do
  version <- Cont withVersionNumber
  date <- Cont withTimestamp
  os <- Cont withOS
  pure $ os ++ "-" ++ show version ++ "-" ++ show date