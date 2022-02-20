{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Chapter7 where

import Data.Data (Typeable, cast)
import Data.Foldable (asum)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Kind (Constraint, Type)
import Data.Maybe (fromMaybe)
import System.IO.Unsafe (unsafePerformIO)
import Prelude

--data Any = forall a. Any a
data Any where
  Any :: a -> Any

-- >>> :t Any True
-- Any True :: Any
-- >>> :t [ Any 5, Any True , Any "hello" ]
-- [ Any 5, Any True , Any "hello" ] :: [Any]

elimAny :: (forall a. a -> r) -> Any -> r
elimAny f (Any a) = f a

-- >>> elimAny (const 1) (Any True)
-- 1

data HasShow where
  HasShow :: Show t => t -> HasShow

-- >>> show [ HasShow 1, HasShow True ]
-- "[1,True]"

elimHasShow ::
  (forall a. Show a => a -> r) ->
  HasShow ->
  r
elimHasShow f (HasShow a) = f a

-- >>> elimHasShow show (HasShow True)
-- "True"

instance Show HasShow where
  show hs = elimHasShow show hs

data Dynamic where
  Dynamic :: Typeable t => t -> Dynamic

elimDynamic ::
  (forall a. Typeable a => a -> r) ->
  Dynamic ->
  r
elimDynamic f (Dynamic a) = f a

fromDynamic :: Typeable a => Dynamic -> Maybe a
fromDynamic = elimDynamic cast

--- >>> fromDynamic (Dynamic (1::Int)) :: Maybe Int
-- Just 1
--- >>> fromDynamic (Dynamic False) :: Maybe Int
-- Nothing

liftD2 ::
  forall a b r.
  (Typeable a, Typeable b, Typeable r) =>
  Dynamic ->
  Dynamic ->
  (a -> b -> r) ->
  Maybe Dynamic
liftD2 d1 d2 f =
  fmap Dynamic . f
    <$> fromDynamic @a d1
    <*> fromDynamic @b d2

pyPlus :: Dynamic -> Dynamic -> Dynamic
pyPlus a b =
  fromMaybe (error "bad types for pyPlus") $
    asum
      [ liftD2 @String @String a b (++),
        liftD2 @Int @Int a b (+),
        liftD2 @String @Int a b $ \strA intB ->
          strA ++ show intB,
        liftD2 @Int @String a b $ \intA strB ->
          show intA ++ strB
      ]

-- >>> fromDynamic @String $ (Dynamic @Int 1) `pyPlus` (Dynamic " ligma")
-- Just "1 ligma"

data Has (c :: Type -> Constraint) where
  Has :: c t => t -> Has c

elimHas ::
  (forall a. c a => a -> r) ->
  Has c ->
  r
elimHas f (Has a) = f a

type HasShow' = Has Show

type Dynamic' = Has Typeable

isMempty :: (Monoid a, Eq a) => a -> Bool
isMempty a = a == mempty

class (Monoid a, Eq a) => MonoidEq a

instance (Monoid a, Eq a) => MonoidEq a

-- >>> isMempty [True]
-- False
-- >>> elimHas isMempty (Has [ True ] :: Has MonoidEq)
-- False

newtype ST s a = ST
  { unsafeRunST :: a
  }

instance Functor (ST s) where
  fmap f (ST a) = seq a . ST $ f a

instance Applicative (ST s) where
  pure = ST
  ST f <*> ST a = seq f . seq a . ST $ f a

instance Monad (ST s) where
  ST a >>= f = seq a $ f a

newtype STRef s a = STRef
  { unSTRef :: IORef a
  }

newSTRef :: a -> ST s (STRef s a)
newSTRef =
  pure . STRef . unsafePerformIO . newIORef

readSTRef :: STRef s a -> ST s a
readSTRef =
  pure . unsafePerformIO . readIORef . unSTRef

writeSTRef :: STRef s a -> a -> ST s ()
writeSTRef ref =
  pure . unsafePerformIO . writeIORef (unSTRef ref)

modifySTRef :: STRef s a -> (a -> a) -> ST s ()
modifySTRef ref f = do
  a <- readSTRef ref
  writeSTRef ref $ f a

runST ::
  (forall s. ST s a) ->
  a
runST = unsafeRunST

safeExample :: ST s String
safeExample = do
  ref <- newSTRef "hello"
  modifySTRef ref (++ " world")
  readSTRef ref

-- >>> runST safeExample
-- "hello world"
-- >>> runST ( newSTRef True )
-- Couldn't match type ‘a’ with ‘STRef s Bool’
--   because type variable ‘s’ would escape its scope
-- This (rigid, skolem) type variable is bound by
--   a type expected by the context:
--     forall s. ST s a
--   at /home/kaixi/git/thinking-with-types/src/Chapter7.hs:170:8-24
-- Expected type: ST s a
--   Actual type: ST s (STRef s Bool)
