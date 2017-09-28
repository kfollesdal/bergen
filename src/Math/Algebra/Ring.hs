-- TODO
-- @ Documentation
-- @ Add tests to RingTest.hs

{-# LANGUAGE
    NoImplicitPrelude,
    FlexibleInstances,
    UndecidableInstances
#-}

{-|
Module      : Math.Ring
Description : Defenition of Ring for Haskell.
Maintainer  : Kristoffer K. Føllesdal <kfollesdal@gmail.com>
-}

module Math.Algebra.Ring  (
  -- * Rings
  Ring (..),
  CommutativeRing (..)
) where

import Math.Algebra.Monoid
import Math.Algebra.Group
import qualified GHC.Num as N (Num)
-- import GHC.Int (Int)
-- import GHC.Integer (Integer)

class (AbelianGroup r, MultiplicativeMonoid r) => Ring r where

class (AbelianGroup r, CommutativMonoid r) => CommutativeRing r where

instance {-# OVERLAPPABLE #-} (N.Num k) => CommutativeRing k where

-- instance CommutativeRing Integer
--
-- instance CommutativeRing Int
