{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Chapter12 where

import Chapter11
import qualified Chapter11
import Data.Functor.Identity as Id ()
import Data.Kind (Constraint, Type)
import qualified Data.Vector as V
import Fcf (Eval, FindIndex, FromMaybe, Fst, Map, TyEq, type (=<<))
import Fcf.Data.List (Elem)
import GHC.TypeLits
  ( ErrorMessage (ShowType, Text, (:$$:), (:<>:)),
    Nat,
    Symbol,
    TypeError,
  )
import GHC.TypeNats (KnownNat)
import Fcf.Utils (Stuck)

instance
  ( TypeError
      ( Text
          "Attempting to show a function of type `"
          :<>: ShowType (a -> b)
          :<>: Text "'"
          :$$: Text "Did you forget to apply an argument?"
      )
  ) =>
  Show (a -> b)
  where
  show = undefined

-- >>> show id
-- Attempting to show a function of type `a0 -> a0'
-- Did you forget to apply an argument?

type family FriendlyFindElem (f :: k -> Type) (t :: k) (ts :: [k]) where
  FriendlyFindElem f t ts =
    FromMaybe
      ( TypeError
          ( 'Text "Attempted to call `friendlyPrj' to produce a `"
              ':<>: 'ShowType (f t)
              ':<>: 'Text "'."
              ':$$: 'Text "But the OpenSum can only contain one of:"
              ':$$: 'Text " "
              ':<>: 'ShowType ts
          )
      )
      =<< FindIndex (TyEq t) ts

friendlyPrj :: forall f t ts. Member t ts => OpenSum f ts -> Maybe (f t)
friendlyPrj = prj

-- >>> let foo = inj ( Identity True ) :: OpenSum Identity '[ Bool , String ]
-- >>> friendlyPrj foo :: Maybe ( Identity Int )
-- Not in scope: type constructor or class ‘Identity’

type family RequireUniqueKey (result :: Bool) (key :: Symbol) (t :: k) (ts :: [(Symbol, k)]) :: Constraint where
  RequireUniqueKey 'True key t ts = ()
  RequireUniqueKey 'False key t ts =
    TypeError
      ( 'Text "Attempting to add a field named `"
          ':<>: 'Text key
          ':<>: 'Text "' with type "
          ':<>: 'ShowType t
          ':<>: 'Text " to an OpenProduct."
          ':$$: 'Text "But the OpenProduct already has a field `"
          ':<>: 'Text key
          ':<>: 'Text "' with type "
          ':<>: 'ShowType (LookupType key ts)
          ':$$: 'Text "The valid fields are "
          ':<>: PPrintListErrorMessage ts
          ':<>: 'Text "."
          ':$$: 'Text "Consider using `update' "
          ':<>: 'Text "instead of `insert'."
      )

insertFriendly ::
  RequireUniqueKey (Eval (UniqueKey key ts)) key t ts =>
  Key key ->
  f t ->
  OpenProduct f ts ->
  OpenProduct f ('(key, t) ': ts)
insertFriendly _ ft (OpenProduct v) =
  OpenProduct $ V.cons (Any ft) v

-- >>> :t insertFriendly #key (Just "world") $ insertFriendly #key (Just "hello") nil
-- Attempting to add a field named `key' with type [Char] to an OpenProduct.
-- But the OpenProduct already has a field `key' with type FromMaybe
--                                                           Stuck
--                                                         =<< Lookup "key" '[ '("key", [Char])]
-- The valid fields are `'("key", [Char])' : nil.
-- Consider using `update' instead of `insert'.

-- 12.1-i
type family RemoveRequireExistingKey (result :: Bool) (key :: Symbol) (ts :: [(Symbol, k)]) :: Constraint where
  RemoveRequireExistingKey 'True key ts = ()
  RemoveRequireExistingKey 'False key ts =
    TypeError
      ( 'Text "Attempting to remove a field named `"
          ':<>: 'Text key
          ':<>: 'Text " to an OpenProduct."
          ':$$: 'Text "But the OpenProduct has no field `"
          ':<>: 'Text key
          ':<>: 'Text "', possible fields are `"
          ':<>: 'ShowType (Eval (Map Fst ts))
          ':<>: 'Text "'."
      )

deleteFriendly ::
  forall key ts t f.
  ( RemoveRequireExistingKey (Eval (Elem key (Eval (Map Fst ts)))) key ts,
    KnownNat (FindElemProd key ts)
  ) =>
  Key key ->
  OpenProduct f ts ->
  OpenProduct f (Eval (DeleteElem key ts))
deleteFriendly = delete

-- >>> :t deleteFriendly #key2 $ insertFriendly #key (Just "hello") nil
-- Attempting to remove a field named `key2 to an OpenProduct.
-- But the OpenProduct has no field `key2', possible fields are `'["key"]'.

type family PPrintListErrorMessage (l :: [a]) :: ErrorMessage where
  PPrintListErrorMessage '[] = 'Text "nil"
  PPrintListErrorMessage (a ': as) = 'Text "`" ':<>: 'ShowType a ':<>: 'Text "' : " ':<>: PPrintListErrorMessage as
  --PPrintListErrorMessage _ = 'Text "ERROR"

