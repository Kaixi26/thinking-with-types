module Chapter3 where

-- +
newtype T1 a = T1 (Int -> a)

instance Functor T1 where
  fmap f (T1 fia) = T1 $ \i -> f (fia i)

-- -
newtype T2 a = T2 (a -> Int)

instance Functor T2 where
  fmap f (T2 fai) = T2 $ \b -> undefined -- nothing to be applied to b

-- (- -> +) = -
newtype T3 a = T3 (a -> a)

instance Functor T3 where
  fmap f (T3 faa) = T3 $ \b -> undefined -- nothing to be applied to b

-- ( -(_ -> +) -> _ ) = -
newtype T4 a = T4 ((Int -> a) -> Int)

instance Functor T4 where
  fmap f (T4 fiai) = T4 $ \i -> undefined -- doesnt work?

-- ( -(- -> _) -> _ ) = +
newtype T5 a = T5 ((a -> Int) -> Int)

instance Functor T5 where
  -- fmap :: (a -> b) -> ((a -> Int) -> Int) -> ((b -> Int) -> Int)
  fmap f (T5 faii) = T5 $
    \bi -> faii $
      \a -> bi $ f a

-- Covariant: Any function 'a -> b' can be lifted into 'T a -> T b'
-- Contravariant: Any function 'a -> b' can be lifted into 'T b -> T a'
-- Invariant: In general, no function 'a -> b' can be lifted into a function over T a

-- >>> :t words
-- >>> :t show
-- >>> :t words . ( show :: Bool -> String )