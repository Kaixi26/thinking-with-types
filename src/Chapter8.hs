{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Chapter8 where

import Data.Coerce (Coercible (..), coerce)
import Data.Foldable (toList)
import qualified Data.Map as M
import Data.Monoid (Product (..), Sum (..))

--newtype ZipList a = ZipList
--  { getZipList :: [a]
--  }
--
--newtype Sum a = Sum
--  { getSum :: a
--  }
-- Same memory representation:
-- [54, 46] -- [Sum 54, Sum 46]
-- ZipList [54, 46] -- ZipList [Sum 54, Sum 46]

-- >>> :t coerce
-- coerce :: Coercible a b => a -> b

fastSum :: [Int] -> Int
fastSum = getSum . mconcat . coerce

-- >>> fastSum [1,2,3,4]
-- 10

newtype Reverse a = Reverse
  { getReverse :: a
  }
  deriving (Eq, Show)

instance Ord a => Ord (Reverse a) where
  compare (Reverse a) (Reverse b) = compare b a

-- >>>  ( M.singleton 'S' True )
-- fromList [('S',True)]
-- >>>  coerce (M.singleton 'S' True) :: M.Map Char ( Reverse Bool )
-- fromList [('S',Reverse {getReverse = True})]
-- >>>  coerce (M.singleton 'S' True) :: M.Map (Reverse Char) Bool
-- Couldn't match type ‘Char’ with ‘Reverse Char’
--   arising from a use of ‘coerce’

data BST v
  = Empty
  | Branch (BST v) v (BST v)

type role BST nominal