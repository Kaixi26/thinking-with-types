{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Chapter2 where

import GHC.TypeLits

-- >>> :k (->)
-- (->) :: * -> * -> *

-- 2.1.3-i
-- >>> :k Show
-- Show :: * -> Constraint

-- 2.1.3-ii
-- >>> :k Functor
-- Functor :: (* -> *) -> Constraint

-- 2.1.3-iii
-- >>> :k Monad
-- Monad :: (* -> *) -> Constraint

-- >>> :k AppendSymbol
-- AppendSymbol :: Symbol -> Symbol -> Symbol
-- >>> :k CmpSymbol
-- CmpSymbol :: Symbol -> Symbol -> Ordering
-- >>> :k CmpSymbol "sandy" "batman"
-- CmpSymbol "sandy" "batman" :: Ordering

-- >>> :k 69
-- 69 :: Nat
-- >>> :k 69 + 69
-- 69 + 69 :: Nat
-- >>> :kind! 69 + 69
-- 69 + 69 :: Nat
-- = 138

-- >>> :kind [Bool]
-- [Bool] :: *
-- >>> :kind '[Bool]
-- '[Bool] :: [*]
-- >>> :kind '[ 'True ]
-- '[ 'True ] :: [Bool]
-- >>> :kind (Int, String)
-- (Int, String) :: *
-- >>> :kind '(69, "nice")
-- '(69, "nice") :: (Nat, Symbol)

type family Or (x :: Bool) (y :: Bool) :: Bool where
  Or 'True y = 'True
  Or 'False y = y

-- >>> :kind! Or 'True 'True
-- Or 'True 'True :: Bool
-- = 'True

-- 2.4-i
type family Not (x :: Bool) where
  Not 'True = 'False
  Not 'False = 'True

-- >>> :kind! Not 'True
-- Not 'True :: Bool
-- = 'False

type family Foo (x :: Bool) (y :: Bool) :: Bool

type family Bar x y :: Bool -> Bool -> Bool

-- >>> :kind Foo
-- Foo :: Bool -> Bool -> Bool
-- >>> :kind Bar
-- Bar :: * -> * -> Bool -> Bool -> Bool
