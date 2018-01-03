-- TODO
-- @ Documentation
-- @ Add test to GroupTest.hs
-- @ Add postfix operrator (^-) :: g -> g for Group?

{-# LANGUAGE
  NoImplicitPrelude,
  FlexibleInstances,
  UndecidableInstances
#-}

{-|
Module      : Math.Group
Description : Defenition of mathemaical groups.
Maintainer  : Kristoffer K. Føllesdal <kfollesdal@gmail.com>
-}

module Math.Algebra.Group  (
 -- * Groups
 Group (..),
 CommutativGroup (..),
 AbelianGroup (..)
) where

import Math.Algebra.Monoid
import qualified GHC.Num as N (Num, negate)
import GHC.Int (Int)

class (MultiplicativeMonoid g) => Group g where
  invers :: g -> g

class (Group g, CommutativMonoid g) => CommutativGroup g where

instance (Group g, CommutativMonoid g) => CommutativGroup g

class (AddidtativeMonoid a) => AbelianGroup a where
  negate :: a -> a
  (-) :: a -> a -> a
  x - y = x + (negate y)

instance {-# OVERLAPPABLE #-} (N.Num k) => AbelianGroup k where
  negate = (N.negate)

-- instance AbelianGroup Integer where
--   negate = negateInteger
--
-- instance AbelianGroup Int where
--   negate = (N.negate)
