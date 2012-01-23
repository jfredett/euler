{-# LANGUAGE FlexibleInstances, OverlappingInstances, UndecidableInstances, NoMonomorphismRestriction #-}
module PELib.Convergence where

import Data.Ratio

class Group a where
  prod    :: a -> a -> a
  unit    :: a
  inverse :: a -> a

class Group a => Abelian a where
  -- TODO: Add some re-write rules?
  add  :: a -> a -> a
  zero :: a
  add = prod
  zero = unit

subt :: Abelian a => a -> a -> a
subt x y = x `add` (inverse y)

class Abelian a => DenseSpace a where
  density :: a -> a -> a
  one     :: a

convergence :: DenseSpace a => (a -> Ordering) -> a -> [a]
convergence conv_cond guess_i = convergence' conv_cond (conv_cond zero) guess_i one
  where
    convergence' :: DenseSpace a => (a -> Ordering) -> Ordering -> a -> a -> [a]
    convergence' f last_dir curr_guess adj = curr_guess : case (last_dir, curr_dir) of
        -- to low
        (LT, LT) -> recurse (curr_guess `add` adj)         adj
        -- to high
        (GT, GT) -> recurse (curr_guess `subt` adj)        adj
        -- critical points 
        (GT, LT) -> recurse (curr_guess `add` reduce_adj)  reduce_adj
        (LT, GT) -> recurse (curr_guess `subt` reduce_adj) reduce_adj
        _        -> []
        where curr_dir = f curr_guess
              recurse  = convergence' f curr_dir 
              reduce_adj = (adj `density` zero)



instance Floating a => DenseSpace a where
  density x y = (x + y) / 2
  one = 1

instance Num a => Group a where
  prod = (+)
  unit = 0
  inverse x = -x

instance Num a => Abelian a where

instance Integral a => DenseSpace (Ratio a) where
  density x y = (x + y) / 2
  one = 1
