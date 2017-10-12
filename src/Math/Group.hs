{-# LANGUAGE
  NoImplicitPrelude,
  FlexibleInstances,
  UndecidableInstances
#-}


-- Documentation
-- Tests

module Math.Group where

import Math.Monoid
import GHC.Integer (Integer, negateInteger)
import qualified GHC.Num as N (negate)
import GHC.Int (Int)

class (MultiplicativeMonoid g) => Group g where
  invers :: g -> g
  -- add postfix operrator (^-) :: g -> g ?

class (Group g, CommutativMonoid g) => CommutativGroup g where

instance (Group g, CommutativMonoid g) => CommutativGroup g

class (AddidtativeMonoid a) => AbelianGroup a where
  negate :: a -> a
  (-) :: a -> a -> a
  x - y = x + (negate y)

instance AbelianGroup Integer where
  negate = negateInteger

instance AbelianGroup Int where
  negate = (N.negate)
