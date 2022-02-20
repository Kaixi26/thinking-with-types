{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Chapter4 where

import Data.Data

-- broken :: (a -> b) -> a -> b
-- broken f a = apply
--   where
--     apply :: b -- not the same `b` as above ;(
--     apply = f a

working :: forall a b. (a -> b) -> a -> b
working f a = apply
  where
    apply :: b
    apply = f a

-- >>> :t fmap
-- fmap :: Functor f => (a -> b) -> f a -> f b
-- >>> :t fmap @Maybe
-- fmap @Maybe :: (a -> b) -> Maybe a -> Maybe b
-- >>> :t (+) @Int
-- (+) @Int :: Int -> Int -> Int

typeName :: forall a. Typeable a => String
typeName = show . typeRep $ Proxy @a

-- >>> :t typeRep
-- typeRep :: Typeable a => proxy a -> TypeRep
-- >>> :t typeRep (Proxy @Int)
-- typeRep (Proxy @Int) :: TypeRep
-- >>> typeName @Bool
-- "Bool"
-- >>> typeName @String
-- "[Char]"

type family AlwaysUnit a where
  AlwaysUnit a = ()