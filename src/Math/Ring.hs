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

module Math.Ring  (
  -- * Rings
  Ring (..),
  CommutativeRing (..)
) where

import Math.Monoid
import Math.Group
import GHC.Int (Int)
import GHC.Integer (Integer)

class (AbelianGroup r, MultiplicativeMonoid r) => Ring r where

class (AbelianGroup r, CommutativMonoid r) => CommutativeRing r where

instance CommutativeRing Integer

instance CommutativeRing Int
