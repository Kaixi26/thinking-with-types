{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Chapter10 where

import Data.Kind (Constraint, Type)
import Fcf (Pure1)
import Fcf.Data.List (type (++))
import Prelude hiding (fst)

{-
fst :: (a, b) -> a
fst (a, b) = a

data Fst a b = Fst (a, b)

class Eval l t | l -> t where
  eval :: l -> t

instance Eval (Fst a b) a where
  eval (Fst (a, b)) = a

-- >>> eval $ Fst (1,"b")
-- 1

-- 10.1-i listToMaybe :: [a] -> Maybe a
data ListToMaybe a = ListToMaybe [a]

instance Eval (ListToMaybe a) (Maybe a) where
  eval (ListToMaybe []) = Nothing
  eval (ListToMaybe (a : _)) = Just a

-- >>> eval $ ListToMaybe [1]
-- Just 1

data MapList dfb a = MapList (a -> dfb) [a]

instance Eval dfb dft => Eval (MapList dfb a) [dft] where
  eval (MapList f []) = []
  eval (MapList f (a : as)) =
    eval (f a) : eval (MapList f as)
-}

-- >>> eval $ MapList Fst [("hello", 1) , ("world", 2)]
-- ["hello","world"]

type Exp a = a -> Type

type family Eval (e :: Exp a) :: a

data Snd :: (a, b) -> Exp b

-- >>> :t snd
-- >>> :kind Snd
-- snd :: (a, b) -> b
-- Snd :: (a, b) -> b -> *

type instance Eval (Snd '(a, b)) = b

-- >>> :kind! Eval (Snd '(1 ,"hello"))
-- Eval (Snd '(1 ,"hello")) :: Symbol
-- = "hello"

data FromMaybe :: a -> Maybe a -> Exp a

type instance Eval (FromMaybe _1 ('Just a)) = a

type instance Eval (FromMaybe a 'Nothing) = a

-- >>> :kind! Eval (FromMaybe "hello" Nothing)
-- >>> :kind! Eval (FromMaybe "hello" (Just " world"))
-- Eval (FromMaybe "hello" Nothing) :: Symbol
-- = "hello"
-- Eval (FromMaybe "hello" (Just " world")) :: Symbol
-- = " world"

-- 10.2-i
data ListToMaybe :: [a] -> Exp a

-- >>> :kind ListToMaybe '[]
-- ListToMaybe '[] :: a -> *

type instance Eval (ListToMaybe '[]) = 'Nothing

--type instance Eval (ListToMaybe (a ': _1)) = 'Just a

data MapList :: (a -> Exp b) -> [a] -> Exp [b]

-- >>> :kind MapList
-- MapList :: (a -> Exp b) -> [a] -> [b] -> *

-- >>> :kind MapList Snd '[ '(1,"") ]
-- MapList Snd '[ '(1,"") ] :: [Symbol] -> *

type instance Eval (MapList f '[]) = '[]

type instance Eval (MapList f (a ': as)) = Eval (f a) ': Eval (MapList f as)

-- >>> :kind! Eval (MapListK (FromMaybe 0) [ 'Nothing , ('Just 1) ])
-- Eval (MapListK (FromMaybe 0) [ 'Nothing , ('Just 1) ]) :: [Nat]
-- = '[0, 1]

-- 10.2-ii Defunctionalize
-- foldr f b [] = b
-- foldr f b (a : as) = foldr f (f a b) as

data Foldr :: (a -> b -> Exp b) -> b -> [a] -> Exp b

type instance Eval (Foldr _ b '[]) = b

type instance Eval (Foldr f b (a ': as)) = Eval (Foldr f (Eval (f a b)) as)

data Pure :: a -> Exp a

type instance Eval (Pure x) = x

data (=<<) :: (a -> Exp b) -> Exp a -> Exp b

type instance Eval (k =<< e) = Eval (k (Eval e))

infixr 0 =<<

data (<=<) :: (b -> Exp c) -> (a -> Exp b) -> a -> Exp c

type instance Eval ((f <=< g) x) = Eval (f (Eval (g x)))

infixr 1 <=<

type Snd2 = Snd <=< Snd

-- >>> :kind Snd2
-- Snd2 :: (a1, (a2, c)) -> c -> *
-- >>> :kind! Eval (Snd2 '(1 , '(2 , 3) ) )
-- Eval (Snd2 '(1 , '(2 , 3) ) ) :: Nat
-- = 3

data TyEq :: a -> b -> Exp Bool

type instance Eval (TyEq a b) = TyEqImpl a b

type family TyEqImpl (a :: k) (b :: k) :: Bool where
  TyEqImpl a a = 'True
  TyEqImpl a b = 'False

-- >>> :kind! Eval (TyEq Int Bool)
-- Eval (TyEq Int Bool) :: Bool
-- = 'False

data Collapse :: [Constraint] -> Exp Constraint

type instance
  Eval (Collapse '[]) =
    (() :: Constraint)

type instance
  Eval (Collapse (a ': as)) =
    (a, Eval (Collapse as))

type All (c :: k -> Constraint) (ts :: [k]) = Collapse =<< MapList (Fcf.Pure1 c) ts

data Pure1 :: (a -> b) -> a -> Exp b

type instance Eval (Fcf.Pure1 f x) = f x

-- >>> :kind! Eval (All Eq '[ Int, Bool ])
-- Eval (All Eq '[ Int, Bool ]) :: Constraint
-- = (Eq Int, (Eq Bool, () :: Constraint))

data Map :: (a -> Exp b) -> f a -> Exp (f b)

type instance Eval (Map f '[]) = '[]

type instance Eval (Map f (a ': as)) = Eval (f a) ': Eval (Map f as)

type instance Eval (Map f 'Nothing) = 'Nothing

type instance Eval (Map f ('Just a)) = 'Just (Eval (f a))

type instance Eval (Map f ('Left x)) = 'Left x

type instance Eval (Map f ('Right a)) = 'Right (Eval (f a))

-- >>> :kind! Eval (Map Snd ( 'Just '(1 , 2) ) )
-- Eval (Map Snd ( 'Just '(1 , 2) ) ) :: Maybe Nat
-- = 'Just 2
-- >>> :kind! Eval (Map Snd '[ '(1 , 2) ])
-- Eval (Map Snd '[ '(1 , 2) ]) :: [Nat]
-- = '[2]

-- 10.4-i
type instance Eval (Map f '(a, b)) = '(a, Eval (f b))

data Mappend :: a -> a -> Exp a

type instance Eval (Mappend '() '()) = '()

type instance Eval (Mappend (a :: Constraint) (b :: Constraint)) = (a, b)

type instance Eval (Mappend (a :: [k]) (b :: [k])) = Eval (a ++ b)

data Mempty :: k -> Exp k

type instance Eval (Mempty '()) = '()

type instance Eval (Mempty (c :: Constraint)) = (() :: Constraint)

type instance Eval (Mempty (l :: [k])) = '[]