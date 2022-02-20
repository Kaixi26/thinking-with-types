{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
module Chapter1 where

import Data.Either
import Prelude (fst, snd, undefined, ($), (.))

-- 1.4-i
-- a^b * a^c = a^(b+c)
witness14i :: (b -> a, c -> a) -> (Either b c -> a)
witness14i (fl, fr) ebc = case ebc of
  Left b -> fl b
  Right c -> fr c

witness14i2 :: (Either b c -> a) -> (b -> a, c -> a)
witness14i2 f = (f . Left, f . Right)

-- 1.4-ii
-- (a * b)^c = a^c + b^c
witness14ii :: (c -> (a, b)) -> (c -> a, c -> b)
witness14ii f = (fst . f, snd . f)

-- 1.4-iii
-- (a^b)^c = a^(b*c)
witness14iii :: (c -> (b -> a)) -> ((b, c) -> a)
witness14iii f (b, c) = f c b
