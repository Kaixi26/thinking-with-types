{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Chapter5 where

import Data.Kind (Constraint, Type)
import Data.List (intercalate)
import Prelude hiding (showList)

five :: Int
five = 5

five_ :: (Int ~ a) => Int
five_ = 5

-- >>> :t five
-- >>> :t five_
-- five :: Int
-- five_ :: Int

data Expr a where
  LitInt :: Int -> Expr Int
  LitBool :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int
  Not :: Expr Bool -> Expr Bool
  If :: Expr Bool -> Expr a -> Expr a -> Expr a

-- >>> :t LitInt 1
-- LitInt 1 :: Expr Int

evalExpr :: Expr a -> a
evalExpr (LitInt i) = i
evalExpr (LitBool b) = b
evalExpr (Add x y) = evalExpr x + evalExpr y
evalExpr (Not x) = not $ evalExpr x
evalExpr (If b x y) =
  if evalExpr b
    then evalExpr x
    else evalExpr y

-- >>> evalExpr $ Not (LitBool False)
-- True

-- GADTs extension just provides syntax sugar for TypeEqualities
-- data ShowableData a
--   = (Show a) => ShowableData a
data Expr_ a
  = (a ~ Int) => LitInt_ Int
  | (a ~ Bool) => LitBool_ Bool
  | (a ~ Int) => Add_ (Expr_ Int) (Expr_ Int)
  | (a ~ Bool) => Not_ (Expr_ Bool)
  | If_ (Expr_ Bool) (Expr_ a) (Expr_ a)

data HList (ts :: [Type]) where
  HNil :: HList '[]
  (:#) :: t -> HList ts -> HList (t ': ts)

infixr 5 :#

-- >>> :t True :# HNil
-- True :# HNil :: HList '[Bool]
-- >>> :t Just " hello " :# True :# HNil
-- Just " hello " :# True :# HNil :: HList '[Maybe [Char], Bool]

hLength :: HList ts -> Int
hLength HNil = 0
hLength (_ :# ts) = 1 + hLength ts

-- >>> hLength $ Just " hello " :# True :# HNil
-- 2

hHead :: HList (t ': ts) -> t
hHead (t :# _) = t

-- >>> hHead $ Just " hello " :# True :# HNil
-- Just " hello "

showBool :: HList '[_1, Bool, _2] -> String
showBool (_ :# b :# _ :# HNil) = show b

-- >>> showBool $ "very" :# True :# "poggers" :# HNil
-- "True"

{-
instance Eq (HList '[]) where
  HNil == HNil = True

instance (Eq t, Eq (HList ts)) => Eq (HList (t ': ts)) where
  (a :# as) == (b :# bs) = a == b && as == bs

--  5.3-i
instance Ord (HList '[]) where
  HNil `compare` HNil = EQ

instance (Ord t, Ord (HList ts)) => Ord (HList (t ': ts)) where
  (a :# as) `compare` (b :# bs) =
    case compare a b of
      EQ -> as `compare` bs
      ord -> ord

-- 5.3-ii

class ShowList a where
  showList :: a -> [String]

instance ShowList (HList '[]) where
  showList _ = []

instance (Show t, ShowList (HList ts)) => ShowList (HList (t ': ts)) where
  showList (h :# t) = show h : showList t

instance Show (HList '[]) where
  show HNil = "[]"

instance (Show t, ShowList (HList ts)) => Show (HList (t ': ts)) where
  show hlist = "[ " ++ intercalate ", " (showList hlist) ++ " ]"

-- >>> show $ 1 :# True :# () :# HNil
-- "[ 1, True, () ]"
-}

type family AllEq (ts :: [Type]) :: Constraint where
  AllEq '[] = ()
  AllEq (t ': ts) = (Eq t, AllEq ts)

-- >>> :kind! AllEq '[]
-- AllEq '[] :: Constraint
-- = () :: Constraint
-- >>> :kind! AllEq '[ Int , Bool ]
-- AllEq '[ Int , Bool ] :: Constraint
-- = (Eq Int, (Eq Bool, () :: Constraint))

type family All (c :: Type -> Constraint) (ts :: [Type]) :: Constraint where
  All c '[] = ()
  All c (t ': ts) = (c t, All c ts)

instance All Eq ts => Eq (HList ts) where
  HNil == HNil = True
  (a :# as) == (b :# bs) = a == b && as == bs

--instance All Ord ts => Ord (HList ts) where
--  HNil `compare` HNil = EQ
--  (a :# as) `compare` (b :# bs) = undefined
  --(a :# as) `compare` (b :# bs) =
  --  case undefined of
  --    EQ -> as `compare` bs
  --    ord -> ord

-- >>> (1 :# True :# () :# HNil) == (1 :# True :# () :# HNil)
-- True
