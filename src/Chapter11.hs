{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Chapter11 where

import Data.Functor.Identity as Id
import Data.Kind (Type)
import Data.Proxy
import qualified Data.Vector as V
import Fcf hiding (Any)
import Fcf.Class.Functor (FMap)
import Fcf.Data.List (Cons, Drop, Reverse, Take)
import GHC.TypeLits
import Unsafe.Coerce
import GHC.OverloadedLabels (IsLabel (fromLabel))

data OpenSum (f :: k -> Type) (ts :: [k]) where
  UnsafeOpenSum ::
    Int ->
    f t ->
    OpenSum f ts

-- >>> :kind! OpenSum Identity '[ Bool ]
-- OpenSum Identity '[ Bool ] :: *
-- = OpenSum Identity '[Bool]

-- >>> :t Id.Idenity
-- Not in scope: data constructor ‘Id.Idenity’
-- Perhaps you meant ‘Id.Identity’ (imported from Data.Functor.Identity)

-- >>> :t UnsafeOpenSum 0 (Identity @Int)
-- Data constructor not in scope: Identity :: f t0

type FindElem (key :: k) (ts :: [k]) =
  FromMaybe Stuck =<< FindIndex (TyEq key) ts

type Member t ts = KnownNat (Eval (FindElem t ts))

findElem :: forall t ts. Member t ts => Int
findElem = fromIntegral . natVal $ Proxy @(Eval (FindElem t ts))

inj :: forall f t ts. Member t ts => f t -> OpenSum f ts
inj = UnsafeOpenSum (findElem @t @ts)

prj :: forall f t ts. Member t ts => OpenSum f ts -> Maybe (f t)
prj (UnsafeOpenSum i f) =
  if i == findElem @t @ts
    then Just $ unsafeCoerce f
    else Nothing

decompose :: OpenSum f (t ': ts) -> Either (f t) (OpenSum f ts)
decompose (UnsafeOpenSum 0 t) = Left $ unsafeCoerce t
decompose (UnsafeOpenSum n t) = Right $ UnsafeOpenSum (n - 1) t

weaken :: OpenSum f ts -> OpenSum f (x ': ts)
weaken (UnsafeOpenSum n t) = UnsafeOpenSum (n + 1) t

-- >>> :t UnsafeOpenSum 0 []
-- UnsafeOpenSum 0 [] :: OpenSum [] ts

-- >>> :t weaken @_ @_ @Bool $ weaken @_ @_ @Int $ UnsafeOpenSum 0 []
-- weaken @_ @_ @Bool $ weaken @_ @_ @Int $ UnsafeOpenSum 0 [] :: OpenSum [] (Bool : Int : _)

-- >>> :t decompose $ weaken @_ @_ @Bool $ weaken @_ @_ @Int $ UnsafeOpenSum 0 []
-- decompose $ weaken @_ @_ @Bool $ weaken @_ @_ @Int $ UnsafeOpenSum 0 [] :: Either [Bool] (OpenSum [] (Int : _))

match :: forall f ts b. (forall t. f t -> b) -> OpenSum f ts -> b
match fn (UnsafeOpenSum _ t) = fn t

data Any (f :: k -> Type) where
  Any :: f t -> Any f

data OpenProduct (f :: k -> Type) (ts :: [(Symbol, k)]) where
  OpenProduct :: V.Vector (Any f) -> OpenProduct f ts

nil :: OpenProduct f '[]
nil = OpenProduct V.empty

data Key (key :: Symbol) = Key

insert ::
  Eval (UniqueKey key ts) ~ 'True =>
  Key key ->
  f t ->
  OpenProduct f ts ->
  OpenProduct f ('(key, t) ': ts)
insert _ ft (OpenProduct v) =
  OpenProduct $ V.cons (Any ft) v

-- >>> let result = insert (Key @"key") (Just "hello") nil
-- >>> :t result
-- >>> :t insert ( Key @"another") ( Just True ) result
-- result :: OpenProduct Maybe '[ '("key", [Char])]
-- insert ( Key @"another") ( Just True ) result :: OpenProduct Maybe '[ '("another", Bool), '("key", [Char])]

type UniqueKey (key :: k) (ts :: [(k, t)]) =
  Null =<< Filter (TyEq key <=< Fst) ts

-- >>> let result = insert (Key @"key") (Just "hello") nil
-- >>> :t insert ( Key @ "key") ( Just True ) result
-- Couldn't match type ‘'False’ with ‘'True’
--   arising from a use of ‘insert’

type FindElemProd (key :: Symbol) (ts :: [(Symbol, k)]) =
  Eval (FromMaybe Stuck =<< FindIndex (TyEq key <=< Fst) ts)

findElemProd :: forall key ts. KnownNat (FindElemProd key ts) => Int
findElemProd = fromIntegral . natVal $ Proxy @(FindElemProd key ts)

type LookupType (key :: k) (ts :: [(k, t)]) =
  FromMaybe Stuck =<< Lookup key ts

get ::
  forall key ts f.
  KnownNat (FindElemProd key ts) =>
  Key key ->
  OpenProduct f ts ->
  f (Eval (LookupType key ts))
get _ (OpenProduct v) =
  unAny $ V.unsafeIndex v $ findElemProd @key @ts
  where
    unAny (Any a) = unsafeCoerce a

type UpdateElem (key :: Symbol) (t :: k) (ts :: [(Symbol, k)]) =
  SetIndex (FindElemProd key ts) '(key, t) ts

update ::
  forall key ts t f.
  KnownNat (FindElemProd key ts) =>
  Key key ->
  f t ->
  OpenProduct f ts ->
  OpenProduct f (Eval (UpdateElem key t ts))
update _ ft (OpenProduct v) =
  OpenProduct $ v V.// [(findElemProd @key @ts, Any ft)]

type DeleteElem (key :: Symbol) (ts :: [(Symbol, k)]) =
  Eval (Take (FindElemProd key ts) ts) ++ Eval (Drop (FindElemProd key ts GHC.TypeLits.+ 1) ts)

-- >>> :i DeleteKey "420" []
-- Not in scope: ‘DeleteKey’

-- 11.3-i
delete ::
  forall key ts t f.
  KnownNat (FindElemProd key ts) =>
  Key key ->
  OpenProduct f ts ->
  OpenProduct f (Eval (DeleteElem key ts))
delete _ (OpenProduct v) =
  OpenProduct $ V.ifilter (\i _ -> i /= findElemProd @key @ts) v

-- >>> let result = insert (Key @"key") (Just "hello") nil
-- >>> let result = insert (Key @"key3") (Just "nice") $ insert (Key @"key2") (Just "69") $ insert (Key @"key") (Just "hello") nil
-- >>> :t result
-- >>> :t delete (Key @"key2") result
-- result
--   :: OpenProduct
--        Maybe '[ '("key3", [Char]), '("key2", [Char]), '("key", [Char])]
-- delete (Key @"key2") result :: OpenProduct Maybe '[ '("key3", [Char]), '("key", [Char])]

-- 11.3-ii proposed solution (wtf)
data Placeholder1Of3 :: (a -> b -> c -> Exp r) -> b -> c -> a -> Exp r

type instance
  Eval (Placeholder1Of3 f b c a) =
    Eval (f a b c)

type UpsertElem (key :: Symbol) (t :: k) (ts :: [(Symbol, k)]) =
  FromMaybe ('(key, t) ': ts)
    =<< Map (Placeholder1Of3 SetIndex '(key, t) ts)
    =<< FindIndex (TyEq key <=< Fst) ts

type UpsertLoc (key :: Symbol) (ts :: [(Symbol, k)]) =
  Eval (FindIndex (TyEq key <=< Fst) ts)

class FindUpsertElem (a :: Maybe Nat) where
  upsertElem :: Maybe Int

instance FindUpsertElem 'Nothing where
  upsertElem = Nothing

instance KnownNat n => FindUpsertElem ('Just n) where
  upsertElem =
    Just . fromIntegral . natVal $ Proxy @n

upsert ::
  forall key ts t f.
  FindUpsertElem (UpsertLoc key ts) =>
  Key key ->
  f t ->
  OpenProduct f ts ->
  OpenProduct f (Eval (UpsertElem key t ts))
upsert k ft (OpenProduct v) =
  OpenProduct $ case upsertElem @(UpsertLoc key ts) of
    Nothing -> V.cons (Any ft) v
    Just n -> v V.// [(n, Any ft)]

instance (key ~ key') => IsLabel key (Key key') where
  fromLabel = Key

-- >>> let result = insert #key3 (Just "nice") $ insert #key4 (Just "69") $ insert #c69 (Just "hello") nil
-- >>> :t result
-- Variable not in scope:
--   (#)
--     :: OpenProduct Maybe ('("key3", [Char]) : '("key4", [Char]) : ts0)
--        -> t1 -> t
