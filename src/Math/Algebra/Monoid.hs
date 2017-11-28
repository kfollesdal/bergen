-- TODO
-- @ Add test for mproduxt and sum
-- @ Change u to one?
-- @ Replace * with ⋅
-- @ Multiplicative, Additative and Commutative monoid for functions a -> a

{-# LANGUAGE
  NoImplicitPrelude
#-}

{-|
Module      : Math.Monoid
Description : Monoids with syntax suitable for mathematics.
Maintainer  : Kristoffer K. Føllesdal <kfollesdal@gmail.com>

__Note:__ Haskell already have definition of monoid in "Data.Monoid". But the
syntax is not good for mathematics. In this module we define monoids with syntax
sutable for mathematics.
-}

module Math.Algebra.Monoid (
  -- * Monoids
  -- | __Definition:__ A /monoid/ \((M,\bullet,u)\) is a set \(M\) togheter with
  -- an 'Math.Properties.Operations.assosiative' binary operation \(\bullet\)
  -- and an element \(u\) which is a 'Math.Properties.Elements.unit'
  -- for \(\bullet\).
  MultiplicativeMonoid (..),
  mproduct,

  -- ** Commutative monoids
  -- __Definition:__  A /commutative monoid/ is a monoid where the binary
  -- opertion also is 'commutative'.
  CommutativMonoid (..),
  AddidtativeMonoid (..),
  sum
  ) where

import GHC.Integer (Integer, timesInteger, plusInteger)
import qualified GHC.Num as N ((+),(*))
import GHC.Int (Int)
import Data.List (foldr)

-- | A monoid ('m','*','e')  with '*' as monoid operation. Must satisfy the
-- following properties:
--
--  prop> e * x = x = x * e  (unit laws)
--  prop> (x * y) * z = x * (y * z) (associative law)
class MultiplicativeMonoid m where
  u :: m
  (*) :: m -> m -> m

-- | Product of a list of elements in a 'MultiplicativeMonoid'.
--
-- >>> mproduct [2,4,6,8]
-- 384
mproduct :: (MultiplicativeMonoid m) => [m] -> m
mproduct = foldr (*) u

instance MultiplicativeMonoid Integer where
  u = 1
  (*) = timesInteger

instance MultiplicativeMonoid Int where
  u = 1
  (*) = (N.*)


-- | A 'MultiplicativeMonoid' is a commutative monoid if:
--
-- prop> x * y = y * x
class (MultiplicativeMonoid m) => CommutativMonoid m where

instance CommutativMonoid Integer
instance CommutativMonoid Int


-- | A commutative monoid (m,'+','zero') with '+' as monoid operation. Must
-- satisfy the following properties:
--
-- prop> zero + x = x = x + zero  (unit laws)
-- prop> (x + y) + z = x + (y + z) (associative law)
-- prop> x + y = y + x (commutative law)
class AddidtativeMonoid m where
  zero :: m
  (+) :: m -> m -> m

-- | Sum of a list of elements in a 'AddidtativeMonoid'.
--
-- >>> sum [2,4,6]
-- 20
sum :: (AddidtativeMonoid c) => [c] -> c
sum = foldr (+) zero

instance AddidtativeMonoid Integer where
  zero = 0
  (+) = plusInteger

instance AddidtativeMonoid Int where
  zero = 0
  (+) = (N.+)
